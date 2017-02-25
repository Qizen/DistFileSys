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
  :<|> mkdir
  :<|> ls
  :<|> createFile
  :<|> openFile
  :<|> returnUsers

fileApi :: Proxy FileApi
fileApi = Proxy

users :: ClientM [User]
getFile :: Maybe String -> ClientM [DfsFile]
postFile :: DfsFile -> ClientM Bool
listFiles :: ClientM [DfsDateName]


users :<|> getFile :<|> postFile :<|> listFiles = client fileApi

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

mkdir :: Maybe String -> Maybe String -> Handler Bool
mkdir path foo = return False

ls :: Maybe String -> Handler [DfsDirContents]
ls path = do
  liftIO $ print "Calling ls"
  return $ [DfsDirContents "foo" False]

createFile :: DfsFile -> Handler Bool
createFile file = do
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

openFile :: Maybe String -> Handler (Maybe DfsFile)
openFile (Just path) = do
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
openFile Nothing =
  return Nothing

returnUsers :: Handler [User]
returnUsers = do
  liftIO $ print "calling users"
  return userlist

userlist :: [User]
userlist = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
