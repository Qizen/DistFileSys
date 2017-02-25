{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
--import Data.Bson.Generic
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import GHC.Generics
import System.IO
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.Directory
import CommonApi

filePath = "FileStore/"
dirServerIp = "127.0.0.1"
dirServerPort = 12345

ourPort :: Int
ourPort = 8080

startApp :: IO ()
startApp = do
   forkIO register
   run ourPort app

register :: IO ()
register = do
  threadDelay $ 5 * 1000000
  print "registering with DirServer"
  manager <- newManager defaultManagerSettings
  runClientM (registerFileServer (Just ourPort)) (ClientEnv manager (BaseUrl Http dirServerIp dirServerPort "" ))
  return ()

app :: Application
app = serve api server

dirApi :: Proxy DirApi
dirApi = Proxy

registerFileServer :: Maybe Int -> ClientM String
mkdir :: Maybe String -> Maybe String -> ClientM Bool
ls :: Maybe String -> ClientM [DfsDirContents]
createFile :: Maybe String -> ClientM Bool
users :: ClientM [User]

registerFileServer :<|> mkdir :<|> ls :<|> createFile :<|> users = client dirApi

api :: Proxy FileApi
api = Proxy

server :: Server FileApi
server =  return users'
    :<|> getFile
    :<|> postFile
    :<|> listFiles
  where
   -- users :: Handler [File]
   -- users = return users'
    getFile :: Maybe String -> Handler [DfsFile]
    getFile Nothing = do return [DfsFile "ERROR" "ERROR" "ERROR"]
    getFile (Just fn) = do
      let fullPath = filePath ++ fn
      contents <- liftIO $ readFile fullPath
      date <- liftIO $ getModificationTime fullPath
      return [DfsFile contents (show date) fullPath]
      --do return [DfsFile "Foo Bar Baz: An Apocalyptic Love Story" "2017/01/14" "foob.txt"]

    postFile :: DfsFile -> Handler Bool
    postFile f@DfsFile{f_contents = c, f_lastModified = lm, f_name = n} = liftIO $ do
      writeFile (filePath ++ n) c
      return True

    listFiles :: Handler [DfsDateName]
    listFiles = liftIO $ do
      fs <- listDirectory filePath
      dns <- mapM (\f -> do
               t <- getModificationTime (filePath ++ f)
               return $ DfsDateName (show t) f) fs
      return dns
      
      
users' :: [User]
users' = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
        
