
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

module CommonApi where

import Data.Aeson
import Data.Aeson.TH
import Data.Bson.Generic
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import System.IO
import Control.Monad.IO.Class
import System.Directory

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           System.Environment           (getArgs, getProgName, lookupEnv)

dirServerIp = "127.0.0.1"
dirServerPort = 12345 :: Int
authServerIp = "127.0.0.1"
authServerPort = 28645 :: Int

-- Data Types

data DfsFile = DfsFile
  { f_contents      :: String
  , f_lastModified  :: String
  , f_name          :: String
  } deriving (Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON, Show)

data DfsDirContents = DfsDirContents
  { dc_name         :: String
  , dc_isFolder     :: Bool
  } deriving (Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON, Show)

data DfsDateName = DfsDateName
  { dndate          :: String
  , dnname          :: String
  } deriving (Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON, Show)

data DfsFileRef = DfsFileRef
  { fr_mData :: DfsDateName
  , fr_serv  :: DfsServRef
  } deriving (Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON, Show)

data DfsServRef = DfsServRef
  { sr_ip   :: String
  , sr_port :: String
  } deriving (Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON, Show)

data DfsToken = DfsToken
  { t_user :: String
  , t_expiry :: String
  } deriving (Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON, Show, Read)

deriving instance FromBSON String
deriving instance ToBSON String

deriving instance FromBSON Bool
deriving instance ToBSON Bool

-- APIs
type FileApi = "getFile"   :> QueryParam "name" String :> Get '[JSON] [DfsFile]
  :<|> "postFile"  :> ReqBody '[JSON] DfsFile  :> Post '[JSON] Bool
  :<|> "listFiles" :> Get '[JSON] [DfsDateName]

type DirApi = "registerFileServer" :> RemoteHost :> QueryParam "port" Int :> Get '[JSON] String
   :<|> "ls" :> QueryParam "token" String :> QueryParam "path" String :> Get '[JSON] [DfsDirContents]
  :<|> "createFile" :> ReqBody '[JSON] (DfsFile, DfsToken) :> Post '[JSON] Bool
  :<|> "openFile" :> QueryParam "token" String :> QueryParam "path" String :> Get '[JSON] (Maybe DfsFile)
  :<|> "lockFile" :> QueryParam "token" String :> QueryParam "path" String :> Get '[JSON] String
  :<|> "unlockFile" :> QueryParam "token" String :> QueryParam "path" String :> Get '[JSON] String
  
type AuthApi = "createUser" :> QueryParam "username" String :> QueryParam "password" String :> Get '[JSON] Bool
  :<|> "login" :> QueryParam "username" String :> QueryParam "password" String :> Get '[JSON] (Either String DfsToken)
  
-- MongoDB Functions from use-haskell
-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  Database.MongoDB.close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = return "127.0.0.1"

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = return $ (27017 :: Integer) -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = return "DFS_DB"
