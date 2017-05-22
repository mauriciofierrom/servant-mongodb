{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module EnvironmentSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Enterprise.API hiding (getEnvironments, getEnvironmentById, addEnvironment, deleteEnvironment)
import Common.Types
import Enterprise.Types
import Test

getEnvironments :: ClientM [Environment]
getEnvironmentById :: String -> ClientM Environment
addEnvironment :: Environment -> ClientM Environment
deleteEnvironment :: String -> ClientM NoContent

getEnvironments :<|> getEnvironmentById :<|> addEnvironment :<|> deleteEnvironment = client environmentApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/environment" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "has environments Prueba and Producción" $ \env -> do
          environments <- try env getEnvironments
          isJust (find (\x -> envDescription x == "Prueba") environments) &&
            isJust (find (\x -> envDescription x == "Producción") environments) `shouldBe` True
          -- length environments `shouldBe` 2

        it "allows to create a new environment" $ \ env -> do
          _ <- try env $ addEnvironment (Environment "59884e90b75c5d1662000000" "New One" 3)
          environments <- try env getEnvironments
          isJust (find (\x -> envDescription x == "New One") environments) `shouldBe` True

        it "allows to show environments by id" $ \ env -> do
          environment <- try env (getEnvironmentById "59884e90b75c5d1662000000")
          envDescription environment `shouldBe` "New One"

        it "allows to update an environment" $ \env -> do
          environment <- try env (getEnvironmentById "59884e90b75c5d1662000000")
          _ <- try env $ addEnvironment $ Environment (envId environment) "Updated" (envCode environment)
          environments <- try env getEnvironments
          let exists = isJust (find (\x -> envDescription x == "Updated" && envCode x == 3) environments)
          let count = length $ filter (\x -> envDescription x == "Updated" && envCode x == 3) environments
          exists && (count == 1) `shouldBe` True

        it "allows to delete an environment" $ \env -> do
          environment <- try env (getEnvironmentById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteEnvironment (envId environment)
          try env (getEnvironmentById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "throws a 404 error for missing environments" $ \ env -> do
          try env (getEnvironmentById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
