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
import Data.Time.Clock
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

loop :: Maybe DfsToken -> InputT IO ()
loop mToken = do
  a <- getInputLine "Enter an Action:\n"
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
    Just "uploadFile" -> do
      case mToken of
        Nothing -> outputStrLn "You are not logged in" >> loop mToken
        Just t -> runUploadFile t >> loop mToken
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
    Just "ls" -> do
      case mToken of
        Nothing -> outputStrLn "You are not logged in" >> loop mToken
        Just t -> runLs t >> loop mToken
    Just "quit" -> return ()
    _ -> do
      outputStrLn "Not a valid option"
      loop mToken

runCreateUser :: InputT IO ()
runCreateUser = do
  u <- getInputLine "Enter a username:\n"
  p <- getPassword (Just '*') "Enter a password:\n"
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ runClientM (createUser (u) (p)) (ClientEnv manager (BaseUrl Http authServerIp authServerPort ""))
  return ()

runLogin :: InputT IO (Maybe DfsToken)
runLogin = do
  u <- getInputLine "Enter a username:\n"
  p <- getPassword (Just '*') "Enter a password:\n"
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
      outputStrLn $ "SUCCESS! Token: " ++ (show token)
      return (Just token)

runGetFile :: DfsToken -> InputT IO ()
runGetFile t = do
  fn <- getInputLine "Enter a filename:\n"
  let name = escapeFilePath (unMaybeString fn)
  cachedFiles <- liftIO $ withMongoDbConnection $ do
    refs <- find (select ["_id" =: name] "CACHE") >>= drainCursor
    return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFile) refs
  case cachedFiles of
    [] -> do
      outputStrLn "Cache miss, querying server..."
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (openFile (Just (show t)) (Just name)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
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

runUploadFile :: DfsToken -> InputT IO ()
runUploadFile t = do
  mpath <- getInputLine "Enter the file's location\n"
  case mpath of
    Nothing -> outputStrLn "No path provided"
    Just path -> do
       exists <- liftIO $ doesFileExist path
       if (exists) then do
         let s = (splitOn "/" path)
         let name = s!!((length s)-1)
         contents <- liftIO $ readFile path
         date <- liftIO $ getModificationTime path
         uploadPath <- getInputLine "Enter the folder under which you want to store this file on the server"
         let uP = unMaybeString uploadPath
         let safeName = escapeFilePath uP
         let f = DfsFile contents (show date) (uP ++ name)
         checkCacheAndUpload f t
       else outputStrLn "File does not exist"

runWriteFile :: DfsToken -> InputT IO ()
runWriteFile token = do
  inPath <- getInputLine "Enter the path and name of the file\n"
  let path = escapeFilePath (unMaybeString inPath)
  utime <- liftIO $ getCurrentTime
  let time = show utime
  outputStrLn "Enter the contents of the file, terminated by double-enter"
  contents <- getMuchInput "" False
  let f = DfsFile contents time path
  checkCacheAndUpload f token

getMuchInput :: String -> Bool -> InputT IO (String)
getMuchInput prev wasEmpty = do
  line <- getInputLine ""
  let ln = unMaybeString line
  case (ln, wasEmpty) of
    ("", True) -> return prev
    ("", False) -> getMuchInput prev True
    (s, True) -> getMuchInput (prev ++ "\n\n" ++ s) False
    (s, False) -> getMuchInput (prev ++ "\n" ++ s) False
    
escapeFilePath :: String -> String
escapeFilePath s = T.unpack $ T.replace "/" "#" $ T.pack s

unescapeFilePath :: String -> String
unescapeFilePath s = T.unpack $ T.replace "#" "/" $ T.pack s

checkCacheAndUpload :: DfsFile -> DfsToken -> InputT IO ()
checkCacheAndUpload file token = do
  outputStrLn $ "Checking for file " ++ (unescapeFilePath (f_name file)) ++ " in cache"
  cachedFiles <- liftIO $ withMongoDbConnection $ do
    refs <- find (select ["_id" =: (f_name file)] "CACHE") >>= drainCursor
    return $ catMaybes $ map (\b -> fromBSON b :: Maybe DfsFile) refs
  case cachedFiles of
    [] -> do
      outputStrLn "Nothing in cache, proceeding with upload"
      manager <- liftIO $ newManager defaultManagerSettings
      result <- liftIO $ runClientM (createFile (file, token)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case result of
        Left e -> outputStrLn $ show e
        Right r -> do
          if r then outputStrLn "Successfully uploaded" else outputStrLn "ERROR uploading, maybe file is locked?"
    _ -> do
      -- should get modification time only here, not entire file
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (openFile (Just (show token)) (Just (f_name file))) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> outputStrLn $ show e
        Right Nothing -> do
          sUp <- execWriteFile token file
          if sUp  then outputStrLn "Successfully uploaded" else outputStrLn "ERROR uploading"
        Right (Just dFile) -> do
          if (f_lastModified dFile) == (f_lastModified file)
            then do
              outputStrLn "Cached file is same as server file, uploading"
              execWriteFile token file
              return ()
            else do
              ans <- getInputLine "Cached file is out of date, proceed anyway? y/n"
              case ans of
                Just "y" -> execWriteFile token file  >> return ()
                _ -> outputStrLn $ show  dFile
               
unMaybeString :: Maybe String -> String
unMaybeString Nothing = ""
unMaybeString (Just s) = s

runLockFile :: DfsToken -> InputT IO ()
runLockFile t = do
  path <- getInputLine "Enter a filepath:\n"
  case path of
    Nothing -> outputStrLn "No path provided"
    p -> do
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (lockFile (Just (show t)) (p)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> outputStrLn $ show e
        Right r -> outputStrLn r        
  return ()

runUnlockFile :: DfsToken -> InputT IO ()
runUnlockFile t = do
  path <- getInputLine "Enter a filepath:\n"
  case path of
    Nothing -> outputStrLn "No path provided"
    p -> do
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (unlockFile (Just(show t)) (p)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> outputStrLn $ show e
        Right r -> outputStrLn r        
  return ()

execWriteFile :: DfsToken -> DfsFile -> InputT IO Bool
execWriteFile t file = do
  manager <- liftIO $ newManager defaultManagerSettings
  result <- liftIO $ runClientM (createFile (file, t)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
  case result of
    Left e -> outputStrLn (show e) >> return False
    Right True -> cacheAdd file >> outputStrLn "File successfully written" >> return True
    Right False -> outputStrLn "File writing failed, the file may be locked" >> return False


runLs :: DfsToken -> InputT IO ()
runLs t = do
  path <- getInputLine "Enter the folder you'd like to ls:\n"
  case path of
    Nothing -> outputStrLn  "Nothing entered"
    Just p -> do 
      manager <- liftIO $ newManager defaultManagerSettings
      res <- liftIO $ runClientM (ls (Just(show t)) (Just (escapeFilePath p))) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
      case res of
        Left e -> outputStrLn (show e) >> return ()
        Right r -> do
          let folds = [x | x <- r, (dc_isFolder x)]
          let files = [x | x <- r, ((dc_isFolder x) == False)]
          outputStrLn "\nFolders:"
          mapM (\x -> do outputStrLn $ "\t" ++ (dc_name x)) folds
          outputStrLn "Files:"
          mapM (\x -> do outputStrLn $ "\t" ++ (dc_name x)) files
          outputStrLn ""
          return ()
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
mkdir :: Maybe String -> Maybe String -> Maybe String -> ClientM Bool
ls :: Maybe String -> Maybe String -> ClientM [DfsDirContents]
createFile :: (DfsFile, DfsToken) -> ClientM Bool
openFile :: Maybe String -> Maybe String -> ClientM (Maybe DfsFile)
lockFile :: Maybe String -> Maybe String -> ClientM String
unlockFile :: Maybe String -> Maybe String -> ClientM String
users :: ClientM [User]

registerFileServer :<|> mkdir :<|> ls :<|> createFile :<|> openFile :<|> lockFile :<|> unlockFile :<|> users = client dirApi

authApi :: Proxy AuthApi
authApi = Proxy

createUser :: Maybe String -> Maybe String -> ClientM Bool
login :: Maybe String -> Maybe String -> ClientM (Either String DfsToken)

createUser :<|> login = client authApi
