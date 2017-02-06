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


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
