{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric   #-}

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

type FileApi =
       "users" :> Get '[JSON] [User]
  :<|> "getFile"   :> QueryParam "name" String :> Get '[JSON] [DfsFile]
  :<|> "postFile"  :> ReqBody '[JSON] DfsFile  :> Post '[JSON] Bool
  :<|> "listFiles" :> Get '[JSON] [DfsDateName]

type DirApi =
       "registerFileServer" :> RemoteHost :> Post '[JSON] String
  :<|> "mkdir" :> QueryParam "path" String :> QueryParam "name" String :> Post '[JSON] Bool
  :<|> "ls" :> QueryParam "path" String :> Post '[JSON] [DfsDirContents]
  :<|> "createFile" :> QueryParam "path" String :> QueryParam "name" :> Post '[JSON] Bool
