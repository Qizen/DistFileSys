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
    , app
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

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 28645 app

app :: Application
app = serve api server

api :: Proxy AuthApi
api = Proxy

server :: Server AuthApi
server = createUser
  :<|> login
  
createUser :: Maybe String -> Maybe String -> Handler Bool
createUser (Just username) (Just password) = do
  --TODO: check if there's already a user with this name
  liftIO $ withMongoDbConnection $ (upsert (select ["_id" =: username] "USERS") (toBSON password))
  return True
createUser _ _ = do
  return False

login :: Maybe String -> Maybe String -> Handler (Either String String)
login (Just username) (Just password)  = do
  ps <- liftIO $ withMongoDbConnection $ do
    refs <- find (select ["_id" =: username] "USERS") >>= drainCursor
    return $ catMaybes $ map (\b -> fromBSON b :: (Maybe String)) refs
  if ps == []
    then do
      return $ Left "User not found"
    else do
      let p = ps!!0
      if p == password
        then do 
          return $ Right "foo"
        else do
          return $ Left "Incorrect Password"
login _ _ = do
  return $ Left "Incorrect Params"
