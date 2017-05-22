{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Item.API where

import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import Servant

import Common.Types
import Item.Types
import Item.Persistence

type ItemApi =
  "items" :> Get '[JSON] [Item] :<|>
    "item" :> Capture "itemId" String :> Get '[JSON] Item :<|>
      "item" :> ReqBody '[JSON] Item :> Post '[JSON] Item :<|>
        "item" :> Capture "itemId" String :> DeleteNoContent '[JSON] NoContent

type ItemTypeApi =
  "item-types" :> Get '[JSON] [ItemType] :<|>
    "item-type" :> Capture "item-typeId" String :> Get '[JSON] ItemType :<|>
      "item-type" :> ReqBody '[JSON] ItemType :> Post '[JSON] ItemType :<|>
        "item-type" :> Capture "item-typeId" String :> DeleteNoContent '[JSON] NoContent

type TaxRateApi =
  "tax-rates" :> Get '[JSON] [TaxRate] :<|>
    "tax-rate" :> Capture "taxRateId" String :> Get '[JSON] TaxRate :<|>
      "tax-rate" :> ReqBody '[JSON] TaxRate :> Post '[JSON] TaxRate :<|>
        "tax-rate" :> Capture "taxRateId" String :> DeleteNoContent '[JSON] NoContent

type TaxApi =
  "taxes" :> Get '[JSON] [Tax] :<|>
    "tax" :> Capture "taxId" String :> Get '[JSON] Tax :<|>
      "tax" :> ReqBody '[JSON] Tax :> Post '[JSON] Tax :<|>
        "tax" :> Capture "taxId" String :> DeleteNoContent '[JSON] NoContent

type FullItemApi = ItemApi
              :<|> ItemTypeApi
              :<|> TaxApi
              :<|> TaxRateApi

fullItemServer :: AppEnv -> Server FullItemApi
fullItemServer env = itemServer env
                :<|> itemTypeServer env
                :<|> taxServer env
                :<|> taxRateServer env

itemApi :: Proxy ItemApi
itemApi = Proxy

itemTypeApi :: Proxy ItemTypeApi
itemTypeApi = Proxy

taxRateApi :: Proxy TaxRateApi
taxRateApi = Proxy

taxApi :: Proxy TaxApi
taxApi = Proxy

itemServer :: AppEnv -> Server ItemApi
itemServer env =
  enter (runReaderTNat env) $ getItems :<|> getItemById :<|> addItem :<|> deleteItem

getItems :: App [Item]
getItems = do
  items <- findItems
  return $ catMaybes items

getItemById :: String -> App Item
getItemById _id = do
  item <- findItemById _id
  case item of
    Nothing -> (lift . throwE) err404
    Just item' -> return item'

addItem :: Item -> App Item
addItem = insertItem

deleteItem :: String -> App NoContent
deleteItem _id = do
  removeItem _id
  return NoContent

taxRateServer :: AppEnv -> Server TaxRateApi
taxRateServer env =
  enter (runReaderTNat env) $ getTaxRates :<|> getTaxRateById :<|> addTaxRate :<|> deleteTaxRate

getTaxRates :: App [TaxRate]
getTaxRates = do
  taxRates <- findTaxRates
  return $ catMaybes taxRates

getTaxRateById :: String -> App TaxRate
getTaxRateById _id = do
  taxRate <- findTaxRateById _id
  case taxRate of
    Nothing -> (lift . throwE) err404
    Just taxRate' -> return taxRate'

addTaxRate :: TaxRate -> App TaxRate
addTaxRate = insertTaxRate

deleteTaxRate :: String -> App NoContent
deleteTaxRate _id = do
  removeTaxRate _id
  return NoContent

taxServer :: AppEnv -> Server TaxApi
taxServer env =
  enter (runReaderTNat env) $ getTaxes :<|> getTaxById :<|> addTax :<|> deleteTax

getTaxes :: App [Tax]
getTaxes = do
  taxes <- findTaxes
  return $ catMaybes taxes

getTaxById :: String -> App Tax
getTaxById _id = do
  tax <- findTaxById _id
  case tax of
    Nothing -> (lift . throwE) err404
    Just tax' -> return tax'

addTax :: Tax -> App Tax
addTax = insertTax

deleteTax :: String -> App NoContent
deleteTax _id = do
  removeTax _id
  return NoContent

itemTypeServer :: AppEnv -> Server ItemTypeApi
itemTypeServer env =
  enter (runReaderTNat env) $ getItemTypes :<|> getItemTypeById :<|> addItemType :<|> deleteItemType

getItemTypes :: App [ItemType]
getItemTypes = do
  itemTypes <- findItemTypes
  return $ catMaybes itemTypes

getItemTypeById :: String -> App ItemType
getItemTypeById _id = do
  itemType' <- findItemTypeById _id
  case itemType' of
    Nothing -> (lift . throwE) err404
    Just itemType'' -> return itemType''

addItemType :: ItemType -> App ItemType
addItemType = insertItemType

deleteItemType :: String -> App NoContent
deleteItemType _id = do
  removeItemType _id
  return NoContent
