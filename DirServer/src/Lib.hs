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

import Data.Aeson
import Data.Aeson.TH
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import GHC.Generics
import System.IO
import Control.Monad.IO.Class
import System.Directory
import Network.Socket
import Control.Monad.Trans.Except
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
      let refList = map (\file -> DfsFileRef file addrString (show port)) files
      liftIO $ print refList
      --TODO save server too
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

createFile :: Maybe String -> Handler Bool
createFile path = do
  return False

returnUsers :: Handler [User]
returnUsers = do
  liftIO $ print "calling users"
  return userlist

userlist :: [User]
userlist = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
