{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Common.Types

-- import Environment.API
import Item.API
import Enterprise.API
import Bill.API

type CombinedApi = FullEnterpriseApi
              :<|> FullItemApi
              :<|> FullBillApi

fullApi :: Proxy CombinedApi
fullApi = Proxy

combinedServer :: AppEnv -> Server CombinedApi
combinedServer env = fullEnterpriseServer env
            :<|> fullItemServer env
            :<|> fullBillServer env

run :: AppEnv -> IO ()
run env = do
  let port = 3000
      settings =
        setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp env

mkApp :: AppEnv -> IO Application
mkApp env = return $ serve fullApi (combinedServer env)
