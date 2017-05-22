{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Types
  ( DbConfig (..)
  , App
  , AppEnv (..) )
where

import Data.Aeson
import Data.Text (Text)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Servant.Server.Internal.ServantErr (ServantErr)

type App = ReaderT AppEnv (ExceptT ServantErr IO)

data DbConfig =
  DbConfig { dbHostname :: String
           , dbName     :: Text
           , dbUser     :: Text
           , dbPassword :: Text }
           deriving (Show)

instance FromJSON DbConfig where
  parseJSON = withObject "dbConf" $ \o -> do
    dbHostname <- o .: "host"
    dbName     <- o .: "database"
    dbUser     <- o .: "username"
    dbPassword <- o .: "password"
    return DbConfig{..}

data AppEnv =
  AppEnv { dbConfig :: DbConfig
         , logPath  :: String }

instance FromJSON AppEnv where
  parseJSON = withObject "config" $ \o -> do
    logPath    <- o      .: "logPath"
    dbConf     <- o      .: "dbConfig"
    dbHostname <- dbConf .: "host"
    dbName     <- dbConf .: "database"
    dbUser     <- dbConf .: "username"
    dbPassword <- dbConf .: "password"
    let dbConfig = DbConfig {..}
    return (AppEnv dbConfig logPath)
