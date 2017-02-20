{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE FlexibleContexts #-}

module CommonApi where

import Data.Aeson
import Data.Aeson.TH
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

-- Data Types

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data DfsFile = DfsFile
  { f_contents      :: String
  , f_lastModified  :: String
  , f_name          :: String
  } deriving (Generic, ToJSON, FromJSON, Show)

data DfsDirContents = DfsDirContents
  { dc_name         :: String
  , dc_isFolder     :: Bool
  } deriving (Generic, ToJSON, FromJSON, Show)

data DfsDateName = DfsDateName
  { dndate          :: String
  , dnname          :: String
  } deriving (Generic, ToJSON, FromJSON, Show)

$(deriveJSON defaultOptions ''User)

type FileApi = "users" :> Get '[JSON] [User]
  :<|> "getFile"   :> QueryParam "name" String :> Get '[JSON] [DfsFile]
  :<|> "postFile"  :> ReqBody '[JSON] DfsFile  :> Post '[JSON] Bool
  :<|> "listFiles" :> Get '[JSON] [DfsDateName]

type DirApi = "registerFileServer" :>  RemoteHost :> Get '[JSON] String
  :<|> "mkdir" :> QueryParam "path" String :> QueryParam "foldName" String :> Get '[JSON] Bool
  :<|> "ls" :> QueryParam "path" String :> Get '[JSON] [DfsDirContents]
  :<|> "createFile" :> QueryParam "path" String :> Get '[JSON] Bool
  :<|> "users":> Get '[JSON] [User]

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
mongoDbPort = return $ 27017 -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = return "USEHASKELLDB"
