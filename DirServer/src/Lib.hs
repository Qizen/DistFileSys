{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.TH
import Data.Bson.Generic
import Data.Time.Clock
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Text as T
import Database.MongoDB
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.Directory
import qualified System.IO as S
import System.Random

import CommonApi

startApp :: IO ()
startApp = run 12345 app

app :: Application
app = serve api server

api :: Proxy DirApi
api = Proxy

server :: Server DirApi
server = registerFileServer
  :<|> ls
  :<|> createFile
  :<|> openFile
  :<|> lockFile
  :<|> unlockFile

fileApi :: Proxy FileApi
fileApi = Proxy

getFile :: Maybe String -> ClientM [DfsFile]
postFile :: DfsFile -> ClientM Bool
listFiles :: ClientM [DfsDateName]


getFile :<|> postFile :<|> listFiles = client fileApi

registerFileServer :: SockAddr -> Maybe Int -> Handler String
registerFileServer sock@(SockAddrInet _ addr) (Just port) = do
  (Just addrString, _) <- liftIO $ getNameInfo [NI_NUMERICHOST] True False sock
  liftIO $ print $ "registering file server\nAddr: " ++ addrString ++ "\nPort: " ++ (show port)
  manager <- liftIO $ newManager defaultManagerSettings
  fileList <- liftIO $ runClientM (listFiles) (ClientEnv manager (BaseUrl Http addrString (read(show port) :: Int)  ""))
  case fileList of
    Left e -> return "FAILURE"
    Right files -> do 
      liftIO $ print files
      let refList = map (\file -> DfsFileRef file (DfsServRef addrString (show port))) files
      liftIO $ print refList
      
      -- store server
      liftIO $ withMongoDbConnection $ (upsert (select ["_id" =: (addrString ++ ":" ++ (show port))] "SERVERS") $ toBSON ( DfsServRef addrString (show port)))

      --store files
      liftIO $ mapM  (\file -> withMongoDbConnection $ (upsert (select ["_id" =: (dnname (fr_mData file))] "FILES") $ toBSON file)) refList

      --just for testing:
      liftIO $ print "Test"
      retVals <- liftIO $ withMongoDbConnection $ do
        refs <- find (select [] "FILES") >>= drainCursor
        return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFileRef) refs
      liftIO $ print retVals
      return "Success"

registerFileServer _ Nothing = return "ERROR port needs to be defined"

validateToken :: DfsToken -> IO (Bool)
validateToken t = do
  currTime <- getCurrentTime
  if (currTime < (read(t_expiry t)::UTCTime)) then return True
    else return False

ls :: Maybe String -> Maybe String -> Handler [DfsDirContents]
ls (Just token) (Just path) = do
  v <- liftIO $ validateToken (read token :: DfsToken) 
  if v then do
    liftIO $ print "Calling ls"
    let p = if path == "" || (T.isSuffixOf "#" (T.pack path)) then T.pack path else T.pack $ path++"#"
    retVals <- liftIO $ withMongoDbConnection $ do
      -- todo regex escape the path string to avoid weird matches
      refs <- find (select [("_id"::T.Text) =: (Regex (T.append "^" p) "")] "FILES") >>= drainCursor
      return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFileRef) refs
    let names = map (dnname.fr_mData) retVals
    let relNames = map (drop (T.length p)) names
    let result = map createDirCont relNames
    return $ result
  else return $ [DfsDirContents "Error, ticket invalid" False]

createDirCont :: String -> DfsDirContents
createDirCont n = do
  if any (=='#') n
    then do
      let fName = (splitOn "#" n)!!0
      DfsDirContents fName True
    else do
      DfsDirContents n False
    
  
createFile :: (DfsFile, DfsToken) -> Handler Bool
createFile (file, token) = do
  v <- liftIO $ validateToken token 
  if v then do
    writeable <- liftIO $ isUnlockable (t_user token) (f_name file)
    if writeable then do
      servs <- liftIO $ withMongoDbConnection $ do
        retVals <- find (select [] "SERVERS") >>= drainCursor
        return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsServRef) retVals
      liftIO $ print servs
      liftIO $ print (length servs)
      idx <- liftIO $ randomRIO ( 0, ((length servs) - 1))
      liftIO $ print idx
      let serv = (servs !! idx)
      liftIO $ print $ "SERV: " ++ (show serv)
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ runClientM (postFile file) (ClientEnv manager (BaseUrl Http (sr_ip serv) (read(sr_port serv) :: Int)  ""))
      -- TODO should check that the post was successful
      -- store this mapping
      liftIO $ withMongoDbConnection $ (upsert (select ["_id" =: (f_name file)] "FILES") $ toBSON file)
      return True
    else return False
  else return False

openFile :: Maybe String -> Maybe String -> Handler (Maybe DfsFile)
openFile (Just token) (Just path) = do
  v <- liftIO $ validateToken (read token :: DfsToken) 
  if v then do
    -- look up file, see if we have it listed
    -- if so call the fileserver and return the result
    -- if not, return Nothing
    liftIO $ print ("opening file: " ++ path)
    fs <- liftIO $ withMongoDbConnection $ do
      refs <- find (select ["_id" =: path] "FILES") >>= drainCursor
      return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFileRef) refs
    -- here we should see which server to choose
    if fs == []
      then do
        liftIO $ print "fs == []"
        return Nothing
      else do
        liftIO $ print ("fs == " ++ (show fs))
        liftIO $ print (length fs)
        idx <- liftIO $ randomRIO ( 0, ((length fs) - 1))
        liftIO $ print ("fs idx = " ++ (show idx))
        let s = fr_serv (fs!!idx)
        liftIO $ print s
        manager <- liftIO $ newManager defaultManagerSettings
        fileList <- liftIO $ runClientM (getFile (Just path)) (ClientEnv manager (BaseUrl Http (sr_ip s) (read(sr_port s) :: Int)  ""))
        case fileList of
          Left e -> do
            liftIO $ print e
            return Nothing
          Right fl -> do
            liftIO $ print ("fl: " ++ show fl)
            return $ Just (fl!!0)
  else return Nothing

openFile Nothing Nothing =
  return Nothing

lockFile :: Maybe String -> Maybe String -> Handler String
lockFile (Just token) (Just path) = do
  v <- liftIO $ validateToken (read token :: DfsToken) 
  if v then do
    let t = ((read token)::DfsToken)
    liftIO $ print $ (t_user t) ++ " wants to lock " ++ path
    l <- liftIO $ isLocked (t_user t) path
    if l
      then return "FAILURE: File already locked"
      else do
        liftIO $ withMongoDbConnection $ (upsert (select ["_id" =: path] "FILELOCKS") $ toBSON t)
        return "SUCCESS: File locked"
  else return "FAILURE: Ticket invalid" 
lockFile Nothing Nothing = return "FAILURE: No Parameter"

unlockFile :: Maybe String -> Maybe String -> Handler String
unlockFile (Just token) (Just path) = do
  v <- liftIO $ validateToken (read token :: DfsToken) 
  if v then do
    l <- liftIO $ isUnlockable (t_user ((read token)::DfsToken)) path
    if l
      then do
        liftIO $ withMongoDbConnection $ delete (select ["_id" =: path] "FILELOCKS")
        return "SUCCESS: File unlocked"
      else do return "FAILURE: the file is not unlockable"
  else return "FAILURE: Ticket invalid"
unlockFile Nothing Nothing = return "FAILURE: No Parameter"

isUnlockable :: String -> String -> IO (Bool)
isUnlockable user path = do
   ls <- liftIO $ withMongoDbConnection $ do
     refs <- find (select ["_id" =: path] "FILELOCKS") >>= drainCursor
     return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsToken) refs
   case ls of
     [] -> return True
     l -> do
       let t = ls!!0
       return ((t_user t) == user)
  
isLocked :: String -> String -> IO (Bool)
isLocked user path = do
  ls <- liftIO $ withMongoDbConnection $ do
    refs <- find (select ["_id" =: path] "FILELOCKS") >>= drainCursor
    return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsToken) refs
  case ls of
    [] -> return False
    _ -> return $ True
    
