{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BillDetailSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Bill.API hiding ( getBillDetails, getBillDetailById, addBillDetail, deleteBillDetail )
import Bill.Types
import Item.Types
import Common.Types
import Test

getBillDetails :: ClientM [BillDetail]
getBillDetailById :: String -> ClientM BillDetail
addBillDetail :: BillDetail -> ClientM BillDetail
deleteBillDetail :: String -> ClientM NoContent

getBillDetails :<|> getBillDetailById :<|> addBillDetail :<|> deleteBillDetail = client billDetailApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/bill-detail" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new BillDetail" $ \ env -> do
          _ <- try env $ addBillDetail $ BillDetail "59884e90b75c5d1662000000" (Item "59884e90b75c5d1662000000" "New One" (Tax "5989a7da5d37b91d79c07532" "Tax" 2) 2 2.5 10 (TaxRate "5989a7da5d37b91d79c07551" 2 "TaxRate" 2 "59884e90b75c5d1662000000" Nothing Nothing Nothing) (ItemType "59884e90b75c5d1662000000" "ItemType")) 2
          billDetails <- try env getBillDetails
          isJust (find (\x -> bdQuantity x == 2) billDetails) `shouldBe` True

        it "Allows to show BillDetail by id" $ \ env -> do
          billDetail <- try env (getBillDetailById "59884e90b75c5d1662000000")
          bdQuantity billDetail `shouldBe` 2

        it "Allows to update a BillDetail" $ \env -> do
          _ <- try env $ addBillDetail $ BillDetail "59884e90b75c5d1662000000" (Item "59884e90b75c5d1662000000" "Updated" (Tax "5989a7da5d37b91d79c07532" "Tax" 2) 1 2.5 10 (TaxRate "5989a7da5d37b91d79c07551" 2 "TaxRate" 2 "59884e90b75c5d1662000000" Nothing Nothing Nothing) (ItemType "59884e90b75c5d1662000000" "ItemType")) 3
          billDetails <- try env getBillDetails
          let exists = isJust (find (\x -> bdQuantity x == 3) billDetails)
          let count = length $ filter (\x -> bdQuantity x == 3) billDetails
          exists && (count == 1) `shouldBe` True

        it "Allows to delete a BillDetail" $ \env -> do
          billDetail <- try env (getBillDetailById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteBillDetail (bdId billDetail)
          try env (getBillDetailById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing BillDetails" $ \ env ->
          try env (getBillDetailById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
