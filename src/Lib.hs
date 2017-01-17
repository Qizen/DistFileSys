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
import GHC.Generics
import System.IO
import Control.Monad.IO.Class
import System.Directory


filePath = "FileStore/"

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
  :<|> "getFile"   :> QueryParam "name" String :> Get '[JSON] [DfsFile]
  :<|> "postFile"  :> ReqBody '[JSON] DfsFile  :> Post '[JSON] Bool 

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  return users'
    :<|> getFile
    :<|> postFile
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
    postFile f@DfsFile{contents = c, lastModified = lm, name = n} = liftIO $ do
      writeFile (filePath ++ n) c
      return True

users' :: [User]
users' = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
        
data DfsFile = DfsFile
  { contents      :: String
  , lastModified  :: String
  , name          :: String
  } deriving (Generic, ToJSON, FromJSON, Show)
