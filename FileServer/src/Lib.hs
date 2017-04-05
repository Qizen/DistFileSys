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
ls :: Maybe String -> Maybe String -> ClientM [DfsDirContents]
createFile :: (DfsFile, DfsToken) -> ClientM Bool
openFile :: Maybe String -> Maybe String -> ClientM (Maybe DfsFile)
lockFile :: Maybe String -> Maybe String -> ClientM (String)
unlockFile :: Maybe String -> Maybe String -> ClientM (String)

registerFileServer :<|> ls :<|> createFile :<|> openFile :<|> lockFile :<|> unlockFile = client dirApi

api :: Proxy FileApi
api = Proxy

server :: Server FileApi
server = getFile
    :<|> postFile
    :<|> listFiles
  where
    getFile :: Maybe String -> Handler [DfsFile]
    getFile Nothing = do return [DfsFile "ERROR" "ERROR" "ERROR"]
    getFile (Just fn) = do
      let fullPath = filePath ++ fn
      contents <- liftIO $ readFile fullPath
      date <- liftIO $ getModificationTime fullPath
      return [DfsFile contents (show date) fn]
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
