{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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
startApp = do
  print "Select an action"
  a <- getLine
  case a of
    "createUser" -> runCreateUser
    "login" -> runLogin
  --  "getFile" -> runGetFile
    _ -> print "Not a valid option"
  startApp
 -- run 80808 app

runCreateUser :: IO ()
runCreateUser = do
  print "Enter a username:\n"
  u <- getLine
  print "Enter a password:\n"
  p <- getLine
  manager <- newManager defaultManagerSettings
  runClientM (createUser (Just u) (Just p)) (ClientEnv manager (BaseUrl Http (authServerIp) (read(authServerPort)::Int) ""))
  return ()

runLogin :: IO ()
runLogin = do
  print "Enter a username:\n"
  u <- getLine
  print "Enter a password:\n"
  p <- getLine
  manager <- newManager defaultManagerSettings
  res <- runClientM (login (Just u) (Just p)) (ClientEnv manager (BaseUrl Http (authServerIp) (read(authServerPort)::Int) ""))
  case res of
    Left e -> print e
    Right (Left e2) -> print e2
    Right (Right token) -> print $ "SUCCESS! Token: " ++ token
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
