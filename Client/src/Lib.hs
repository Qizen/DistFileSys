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
import Control.Monad
--import Control.Concurrent.STM
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

startApp :: Maybe String -> IO ()
startApp mToken = do
  print "Select an action"
  a <- getLine
  case a of
    "createUser" -> do
      runCreateUser
      startApp mToken
    "login" -> do
      t <- runLogin
      startApp t
    "openFile" -> do
      case mToken of
        Nothing -> do
          print $ "You are not logged in"
          startApp mToken
        Just t -> do
          runGetFile t
          startApp mToken
    "writeFile" -> do
      case mToken of
        Nothing -> do
          print "You are not logged in"
          startApp mToken
        Just t -> do
          runWriteFile t
          startApp mToken
      
    "quit" -> return ()
    _ -> do
      print "Not a valid option"
      startApp mToken

runCreateUser :: IO ()
runCreateUser = do
  print "Enter a username:\n"
  u <- getLine
  print "Enter a password:\n"
  p <- getLine
  manager <- newManager defaultManagerSettings
  runClientM (createUser (Just u) (Just p)) (ClientEnv manager (BaseUrl Http authServerIp authServerPort ""))
  return ()

runLogin :: IO (Maybe String)
runLogin = do
  print "Enter a username:\n"
  u <- getLine
  print "Enter a password:\n"
  p <- getLine
  manager <- newManager defaultManagerSettings
  res <- runClientM (login (Just u) (Just p)) (ClientEnv manager (BaseUrl Http authServerIp authServerPort ""))
  case res of
    Left e -> do
      print e
      return Nothing
    Right (Left e2) -> do
      print e2
      return Nothing
    Right (Right token) -> do
      print $ "SUCCESS! Token: " ++ token
      return (Just token)

runGetFile :: String -> IO ()
runGetFile t = do
  print "Enter a filename"
  fn <- getLine
  cachedFiles <- withMongoDbConnection $ do
    refs <- find (select ["_id" =: fn] "CACHE") >>= drainCursor
    return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFile) refs
  case cachedFiles of
    [] -> do
      print "Cache miss, querying server..."
      manager <- newManager defaultManagerSettings
      res <- runClientM (openFile (Just fn)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> print e
        Right (Just r) -> do
          cacheAdd r
          print r
        _ -> print "No file of that name exists"
    _ -> do
      print "cache hit!"
      print $ cachedFiles!!0
  return ()

runWriteFile :: String -> IO ()
runWriteFile t = do
  print "Enter a filepath"
  path <- getLine
  exists <- doesFileExist path
  if (exists) then do
    let s = (splitOn "/" path)
    let name = s!!((length s)-1)
    print $ "Checking for file " ++ name ++ " in cache"
    cachedFiles <- withMongoDbConnection $ do
      refs <- find (select ["_id" =: name] "CACHE") >>= drainCursor
      return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFile) refs
    case cachedFiles of
      [] -> do
        print "Nothing in cache, proceeding with upload"
        manager <- newManager defaultManagerSettings
        contents <- readFile path
        date <- getModificationTime path
        let f = DfsFile contents (show date) name
        result <- runClientM (createFile f) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
        case result of
          Left e -> print e
          Right r -> do
            if r then print "Successfully uploaded" else print "ERROR uploading"
      _ -> do
        -- should get modification time only here, not entire file
         manager <- newManager defaultManagerSettings
         res <- runClientM (openFile (Just name)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
         case res of
           Left e -> print e
           Right Nothing -> do
             sUp <- execWriteFile path
             if sUp  then print "Successfully uploaded" else print "ERROR uploading"
           Right (Just dFile) -> do
             s <- getModificationTime path
             if (f_lastModified dFile) == (show s)
               then do
                 print "cached file is same as server file, uploading"
                 execWriteFile path
                 return ()
               else do
                 print "cached file is out of date, proceed anyway? y/n"
                 ans <- getLine
                 case ans of
                   "y" -> execWriteFile path >> return ()
                   _ -> print dFile
  else print "File does not exist"

execWriteFile :: String -> IO Bool
execWriteFile path = do
  let s = (splitOn "/" path)
  let name = s!!((length s)-1)
  manager <- newManager defaultManagerSettings
  contents <- readFile path
  date <- getModificationTime path
  let f = DfsFile contents (show date) name
  result <- runClientM (createFile f) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
  case result of
    Left e -> print e >> return False
    Right b -> cacheAdd f >> return b
    
cacheAdd :: DfsFile -> IO ()
cacheAdd f = do
   cacheExists <- withMongoDbConnection $ do
     refs <- allCollections
     return $ any (=="CACHE") refs
   when (cacheExists == False) $ do  
     withMongoDbConnection $ do
       liftIO $ print "cache db missing, creating a new one..."
       --TODO: can't change size of doc when updating a capped collection, so this doesn't work
       -- need to find another way of limiting caching.
       --createCollection [Capped, MaxByteSize 100000, MaxItems 100] "CACHE"
       return ()
   withMongoDbConnection $ upsert (select ["_id" =: (f_name f)] "CACHE") $ toBSON f
   print $ (f_name f) ++ " added to cache"
   return ()

fileApi :: Proxy FileApi
fileApi = Proxy

f_users :: ClientM [User]
getFile :: Maybe String -> ClientM [DfsFile]
postFile :: DfsFile -> ClientM Bool
listFiles :: ClientM [DfsDateName]


f_users :<|> getFile :<|> postFile :<|> listFiles = client fileApi


dirApi :: Proxy DirApi
dirApi = Proxy

registerFileServer :: Maybe Int -> ClientM String
mkdir :: Maybe String -> Maybe String -> ClientM Bool
ls :: Maybe String -> ClientM [DfsDirContents]
createFile :: DfsFile -> ClientM Bool
openFile :: Maybe String -> ClientM (Maybe DfsFile)
users :: ClientM [User]

registerFileServer :<|> mkdir :<|> ls :<|> createFile :<|> openFile :<|> users = client dirApi

authApi :: Proxy AuthApi
authApi = Proxy

createUser :: Maybe String -> Maybe String -> ClientM Bool
login :: Maybe String -> Maybe String -> ClientM (Either String String)

createUser :<|> login = client authApi
