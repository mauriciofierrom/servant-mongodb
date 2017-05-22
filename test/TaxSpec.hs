{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TaxSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Item.API hiding ( getTaxes, getTaxById, addTax, deleteTax )
import Item.Types
import Common.Types
import Test

getTaxes :: ClientM [Tax]
getTaxById :: String -> ClientM Tax
addTax :: Tax -> ClientM Tax
deleteTax :: String -> ClientM NoContent

getTaxes :<|> getTaxById :<|> addTax :<|> deleteTax = client taxApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/tax" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Tax" $ \ env -> do
          _ <- try env $ addTax (Tax "59884e90b75c5d1662000000" "New One" 1)
          taxes <- try env getTaxes
          isJust (find (\x -> taxName x == "New One") taxes) `shouldBe` True

        it "Allows to show Taxes by id" $ \ env -> do
          tax <- try env (getTaxById "59884e90b75c5d1662000000")
          taxName tax `shouldBe` "New One"

        it "Allows to update a Tax" $ \env -> do
          tax <- try env (getTaxById "59884e90b75c5d1662000000")
          _ <- try env $ addTax $ Tax (taxId tax) "Updated" (taxCode tax)
          taxs <- try env getTaxes
          let exists = isJust (find (\x -> taxName x == "Updated" && taxCode x == 1) taxs)
          let count = length $ filter (\x -> taxName x == "Updated" && taxCode x == 1) taxs
          exists && (count == 1) `shouldBe` True

        it "Allows to delete a Tax" $ \env -> do
          tax <- try env (getTaxById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteTax (taxId tax)
          try env (getTaxById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Taxes" $ \ env -> do
          try env (getTaxById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

