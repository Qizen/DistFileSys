{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.Except
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
  manager <- newManager defaultManagerSettings
  res <- runClientM (openFile (Just fn)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort ""))
  print res
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
