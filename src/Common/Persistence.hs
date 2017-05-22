{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Common.Persistence
  ( runDb )
where

import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Database.MongoDB
  ( Action
  , connect
  , close
  , host
  , auth
  , access
  , master )

import Common.Types

runDb :: Action IO a -> App a
runDb a = do
  dbConf <- dbConfig <$> ask
  pipe <- liftIO $ connect (host (dbHostname dbConf))
  -- Have to check if it isn't authorized
  _ <- lift . lift $ access pipe master (dbName dbConf) $ auth (dbUser dbConf) (dbPassword dbConf)
  result <- lift . lift $ access pipe master (dbName dbConf) a
  liftIO $ close pipe
  return result
