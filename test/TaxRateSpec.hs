{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TaxRateSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Item.API hiding ( getTaxRates, getTaxRateById, addTaxRate, deleteTaxRate )
import Item.Types
import Common.Types
import Test

getTaxRates :: ClientM [TaxRate]
getTaxRateById :: String -> ClientM TaxRate
addTaxRate :: TaxRate -> ClientM TaxRate
deleteTaxRate :: String -> ClientM NoContent

getTaxRates :<|> getTaxRateById :<|> addTaxRate :<|> deleteTaxRate = client taxRateApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/tax-rate" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Tax Rate" $ \ env -> do
          _ <- try env $ addTaxRate (TaxRate "59884e90b75c5d1662000000" 1 "New One" 3 "tax_id" (Just 20) Nothing Nothing)
          taxRates <- try env getTaxRates
          isJust (find (\x -> taxRateDescription x == "New One") taxRates) `shouldBe` True

        it "Allows to show Tax Rates by id" $ \ env -> do
          taxRate <- try env (getTaxRateById "59884e90b75c5d1662000000")
          taxRateDescription taxRate `shouldBe` "New One"

        it "Allows to update a Tax Rate" $ \env -> do
          _ <- try env $ addTaxRate $ (TaxRate "59884e90b75c5d1662000000" 1 "Updated" 3 "tax_id" (Just 20) Nothing Nothing)
          taxRates <- try env getTaxRates
          let exists = isJust (find (\x -> taxRateDescription x == "Updated" && taxRateCode x == 1) taxRates)
          let count = length $ filter (\x -> taxRateDescription x == "Updated" && taxRateCode x == 1) taxRates
          exists && (count == 1) `shouldBe` True

        it "Allows to delete a Tax Rate" $ \env -> do
          taxRate <- try env (getTaxRateById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteTaxRate (taxRateId taxRate)
          try env (getTaxRateById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Tax Rates" $ \ env -> do
          try env (getTaxRateById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
