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
import qualified  Network.Wai.Handler.Warp as W
import Servant
import Servant.Client
import System.Directory
import qualified System.IO as S
import System.Random
import System.Console.Haskeline

import CommonApi

startApp :: IO ()
startApp = runInputT defaultSettings (loop Nothing)

loop :: Maybe String -> InputT IO ()
loop mToken = do
  a <- getInputLine "Select an Action\n"
  case a of
    Just "createUser" -> do
      runCreateUser
      loop mToken
    Just "login" -> do
      t <- runLogin
      loop t
    Just "openFile" -> do
      case mToken of
        Nothing -> do
          outputStrLn $ "You are not logged in"
          loop mToken
        Just t -> do
          runGetFile t
          loop mToken
    Just "writeFile" -> do
      case mToken of
        Nothing -> do
          outputStrLn "You are not logged in"
          loop mToken
        Just t -> do
          runWriteFile t
          loop mToken
    Just "lockFile" -> do
      case mToken of
        Nothing -> do
          outputStrLn "You are not logged in"
          loop mToken
        Just t -> do
          runLockFile t
          loop mToken
    Just "unlockFile" -> do
      case mToken of
        Nothing -> do
          outputStrLn "You are not logged in"
          loop mToken
        Just t -> do
          runUnlockFile t
          loop mToken
      
    Just "quit" -> return ()
    _ -> do
      outputStrLn "Not a valid option"
      loop mToken

runCreateUser :: InputT IO ()
runCreateUser = do
  u <- getInputLine "Enter a username:"
  p <- getPassword (Just '*') "Enter a password:"
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ runClientM (createUser (u) (p)) (ClientEnv manager (BaseUrl Http authServerIp authServerPort ""))
  return ()

runLogin :: InputT IO (Maybe String)
runLogin = do
  u <- getInputLine "Enter a username:"
  p <- getPassword (Just '*') "Enter a password:"
  manager <- liftIO $ newManager defaultManagerSettings
  res <- liftIO $ runClientM (login (u) (p)) (ClientEnv manager (BaseUrl Http authServerIp authServerPort ""))
  case res of
    Left e -> do
      outputStrLn $ show e
      return Nothing
    Right (Left e2) -> do
      outputStrLn e2
      return Nothing
    Right (Right token) -> do
      outputStrLn $ "SUCCESS! Token: " ++ token
      return (Just token)

runGetFile :: String -> InputT IO ()
runGetFile t = do
  fn <- getInputLine "Enter a filename"
  cachedFiles <- liftIO $ withMongoDbConnection $ do
    refs <- find (select ["_id" =: fn] "CACHE") >>= drainCursor
    return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFile) refs
  case cachedFiles of
    [] -> do
      outputStrLn "Cache miss, querying server..."
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (openFile (fn)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> outputStrLn $ show e
        Right (Just r) -> do
          cacheAdd r
          outputStrLn $ show r
        _ -> outputStrLn "No file of that name exists"
    _ -> do
      outputStrLn "cache hit!"
      outputStrLn $ show $ cachedFiles!!0
  return ()

runWriteFile :: String -> InputT IO ()
runWriteFile t = do
  mpath <- getInputLine "Enter a filepath"
  case mpath of
    Nothing -> outputStrLn "No path provided"
    Just path -> do
       exists <- liftIO $ doesFileExist path
       if (exists) then do
         let s = (splitOn "/" path)
         let name = s!!((length s)-1)
         outputStrLn $ "Checking for file " ++ name ++ " in cache"
         cachedFiles <- liftIO $ withMongoDbConnection $ do
           refs <- find (select ["_id" =: name] "CACHE") >>= drainCursor
           return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFile) refs
         case cachedFiles of
           [] -> do
             outputStrLn "Nothing in cache, proceeding with upload"
             manager <- liftIO $ newManager defaultManagerSettings
             contents <- liftIO $ readFile path
             date <- liftIO $ getModificationTime path
             let f = DfsFile contents (show date) name
             result <- liftIO $ runClientM (createFile f) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
             case result of
               Left e -> outputStrLn $ show e
               Right r -> do
                 if r then outputStrLn "Successfully uploaded" else outputStrLn "ERROR uploading"
           _ -> do
             -- should get modification time only here, not entire file
             manager <- liftIO $ newManager defaultManagerSettings
             res <- liftIO $ runClientM (openFile (Just name)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
             case res of
               Left e -> outputStrLn $ show e
               Right Nothing -> do
                 sUp <- execWriteFile path
                 if sUp  then outputStrLn "Successfully uploaded" else outputStrLn "ERROR uploading"
               Right (Just dFile) -> do
                 s <- liftIO $ getModificationTime path
                 if (f_lastModified dFile) == (show s)
                   then do
                     outputStrLn "cached file is same as server file, uploading"
                     execWriteFile path
                     return ()
                   else do
                     ans <- getInputLine "cached file is out of date, proceed anyway? y/n"
                     case ans of
                       Just "y" -> execWriteFile path >> return ()
                       _ -> outputStrLn $ show  dFile
         else outputStrLn "File does not exist"

runLockFile :: String -> InputT IO ()
runLockFile t = do
  path <- getInputLine "Enter a filepath"
  case path of
    Nothing -> outputStrLn "No path provided"
    p -> do
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (lockFile (p)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> outputStrLn $ show e
        Right r -> outputStrLn r        
  return ()

runUnlockFile :: String -> InputT IO ()
runUnlockFile t = do
  path <- getInputLine "Enter a filepath"
  case path of
    Nothing -> outputStrLn "No path provided"
    p -> do
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (unlockFile (p)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> outputStrLn $ show e
        Right r -> outputStrLn r        
  return ()

execWriteFile :: String -> InputT IO Bool
execWriteFile path = do
  let s = (splitOn "/" path)
  let name = s!!((length s)-1)
  manager <- liftIO $ newManager defaultManagerSettings
  contents <- liftIO $ readFile path
  date <- liftIO $ getModificationTime path
  let f = DfsFile contents (show date) name
  result <- liftIO $ runClientM (createFile f) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
  case result of
    Left e -> outputStrLn (show e) >> return False
    Right b -> cacheAdd f >> return b
    
cacheAdd :: DfsFile -> InputT IO ()
cacheAdd f = do
   cacheExists <-liftIO $  withMongoDbConnection $ do
     refs <- allCollections
     return $ any (=="CACHE") refs
   liftIO $ when (cacheExists == False) $ do  
     withMongoDbConnection $ do
       liftIO $ print "cache db missing, creating a new one..."
       --TODO: can't change size of doc when updating a capped collection, so this doesn't work
       -- need to find another way of limiting caching.
       --createCollection [Capped, MaxByteSize 100000, MaxItems 100] "CACHE"
       return ()
   liftIO $ withMongoDbConnection $ upsert (select ["_id" =: (f_name f)] "CACHE") $ toBSON f
   outputStrLn $ (f_name f) ++ " added to cache"
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
lockFile :: Maybe String -> ClientM String
unlockFile :: Maybe String -> ClientM String
users :: ClientM [User]

registerFileServer :<|> mkdir :<|> ls :<|> createFile :<|> openFile :<|> lockFile :<|> unlockFile :<|> users = client dirApi

authApi :: Proxy AuthApi
authApi = Proxy

createUser :: Maybe String -> Maybe String -> ClientM Bool
login :: Maybe String -> Maybe String -> ClientM (Either String String)

createUser :<|> login = client authApi
