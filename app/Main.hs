{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Common.Types
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  env <- withConfig
  run env

withConfig :: IO AppEnv
withConfig = do
  configFile <- BL.readFile "config.js"
  (putStrLn . toString) configFile
  appEnv <- pure (decode configFile :: Maybe AppEnv)
  case appEnv of
    Nothing     -> error "Configuration could not be loaded"
    Just env -> return env
