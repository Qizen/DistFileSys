{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import System.IO
import Control.Monad.IO.Class
import System.Directory
import Network.Socket
import CommonApi

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy DirApi
api = Proxy

server :: Server DirApi
server = registerFileServer
  :<|> mkdir
  :<|> ls
  :<|> createFile

registerFileServer :: a -> Handler String
registerFileServer sock = return "NOT IMPLEMENTED"

mkdir :: String -> Maybe String -> Handler Bool
mkdir path foo = return False

ls :: String -> Handler [DfsDirContents]
ls path = return $ [DfsDirContents "foo" False]

createFile :: String -> Handler Bool
createFile path = return False


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
