{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Item.Persistence
  ( findItemById
  , findItems
  , getItem
  , getItemDoc
  , insertItem
  , removeItem
  , findItemTypeById
  , findItemTypes
  , getItemTypeDoc
  , insertItemType
  , removeItemType
  , findTaxById
  , findTaxes
  , getTaxDoc
  , insertTax
  , removeTax
  , findTaxRateById
  , findTaxRates
  , getTaxRateDoc
  , insertTaxRate
  , removeTaxRate )

where

import qualified Data.Bson as B
import Database.MongoDB
  ( Document
  , delete
  , insert
  , save
  , find
  , findOne
  , rest
  , select
  , (=:) )

import Common.Types
import Common.Persistence
import Item.Types

findItemById :: String -> App (Maybe Item)
findItemById _id = do
  itemDoc <- runDb $ findOne (select ["_id" =: (B.ObjId $ read _id)] "items")
  item <- pure $ getItem itemDoc
  return item

findItems :: App [Maybe Item]
findItems = do
  itemDocs <- runDb $ rest =<< find (select [] "items")
  items <- pure $ map getItem (Just <$> itemDocs)
  return items

getItem :: Maybe Document -> Maybe Item
getItem document =
  case document of
     Nothing   -> Nothing
     Just doc  -> Item
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "description" doc
                     <*> getTax (B.lookup "tax" doc)
                     <*> B.lookup "code" doc
                     <*> B.lookup "unit_value" doc
                     <*> B.lookup "discount" doc
                     <*> getTaxRate (B.lookup "tax_rate" doc)
                     <*> getItemType (B.lookup "item_type" doc)

getItemDoc :: Item -> Document
getItemDoc Item{..} = [ "_id" =: B.ObjId (read itemId)
                      , "description" =: itemDescription
                      , "tax" =: getTaxDoc itemTax
                      , "code" =: itemCode
                      , "unit_value" =: itemUnitValue
                      , "discount" =: itemDiscount
                      , "tax_rate" =: getTaxRateDoc itemTaxRate
                      , "item_type" =: getItemTypeDoc itemType]

-- this code is not secure
-- what happens if the insert failed?
-- Maybe pattern match with Failure type
insertItem :: Item -> App Item
insertItem Item{..} =
  case itemId of
    "" -> do
      _id <- runDb $
        insert "items" [ "description" =: itemDescription
                       , "tax" =: [ "_id" =: (B.ObjId $ read (taxId itemTax))
                                  , "name" =: taxName itemTax
                                  , "code" =: taxCode itemTax ]
                       , "code" =: itemCode
                       , "unit_value" =: itemUnitValue
                       , "discount" =: itemDiscount
                       , "tax_rate" =: [ "_id" =: (B.ObjId $ read (taxRateId itemTaxRate))
                                       , "code" =: taxRateCode itemTaxRate
                                       , "description" =: taxRateDescription itemTaxRate
                                       , "group" =: taxRateGroup itemTaxRate
                                       , "tax" =: taxRateTax itemTaxRate
                                       , "percentage" =: taxRatePercentage itemTaxRate
                                       , "rate" =: taxRateRate itemTaxRate
                                       , "unit" =: taxRateUnit itemTaxRate ]
                       , "item_type" =: [ "_id" =: (B.ObjId $ read (itemTypeId itemType))
                                        , "description" =: itemTypeDescription itemType] ]
      return $ Item (show _id) itemDescription itemTax itemCode itemUnitValue itemDiscount itemTaxRate itemType
    _ -> do
      runDb $
        save "items" [ "_id" =: B.ObjId (read itemId)
                     , "description" =: itemDescription
                     , "tax" =: [ "_id" =: (B.ObjId $ read (taxId itemTax))
                                , "name" =: taxName itemTax
                                , "code" =: taxCode itemTax ]
                     , "code" =: itemCode
                     , "unit_value" =: itemUnitValue
                     , "discount" =: itemDiscount
                     , "tax_rate" =: [ "_id" =: (B.ObjId $ read (taxRateId itemTaxRate))
                                     , "code" =: taxRateCode itemTaxRate
                                     , "description" =: taxRateDescription itemTaxRate
                                     , "group" =: taxRateGroup itemTaxRate
                                     , "tax" =: taxRateTax itemTaxRate
                                     , "percentage" =: taxRatePercentage itemTaxRate
                                     , "rate" =: taxRateRate itemTaxRate
                                     , "unit" =: taxRateUnit itemTaxRate ]
                     , "item_type" =: [ "_id" =: (B.ObjId $ read (itemTypeId itemType))
                                      , "description" =: itemTypeDescription itemType] ]
      return Item {..}

removeItem :: String -> App ()
removeItem _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "items"

findItemTypeById :: String -> App (Maybe ItemType)
findItemTypeById _id = do
  itemTypeDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "item_types")
  itemType <- pure $ getItemType itemTypeDoc
  return itemType

findItemTypes :: App [Maybe ItemType]
findItemTypes = do
  itemTypeDocs <- runDb $ rest =<< find (select [] "item_types")
  itemTypes <- pure $ map getItemType (Just <$> itemTypeDocs)
  return itemTypes

getItemType :: Maybe Document -> Maybe ItemType
getItemType document =
  case document of
     Nothing   -> Nothing
     Just doc  -> ItemType
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "description" doc

getItemTypeDoc :: ItemType -> Document
getItemTypeDoc ItemType{..} = [ "_id" =: B.ObjId (read itemTypeId)
                              , "description" =: itemTypeDescription ]

insertItemType :: ItemType -> App ItemType
insertItemType ItemType{..} =
  case itemTypeId of
    "" -> do
      _id <- runDb $
        insert "item_types" ["description" =: itemTypeDescription]
      return $ ItemType (show _id) itemTypeDescription
    _ -> do
      runDb $
        save "item_types" ["_id" =: B.ObjId (read itemTypeId), "description" =: itemTypeDescription]
      return ItemType {..}

removeItemType :: String -> App ()
removeItemType _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "item_types"

findTaxById :: String -> App (Maybe Tax)
findTaxById _id = do
  taxDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "taxes")
  tax <- pure $ getTax taxDoc
  return tax

findTaxes :: App [Maybe Tax]
findTaxes = do
  taxDocs <- runDb $ rest =<< find (select [] "taxes")
  taxes <- pure $ map getTax (Just <$> taxDocs)
  return taxes

getTax :: Maybe Document -> Maybe Tax
getTax document =
  case document of
     Nothing   -> Nothing
     Just doc  -> Tax
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "name" doc
                     <*> B.lookup "code" doc

getTaxDoc :: Tax -> Document
getTaxDoc Tax{..} = [ "_id" =: B.ObjId (read taxId)
                    , "code" =: taxCode
                    , "name" =: taxName ]

insertTax :: Tax -> App Tax
insertTax Tax{..} =
  case taxId of
    "" -> do
      _id <- runDb $
        insert "taxes" ["code" =: taxCode , "name" =: taxName]
      return $ Tax (show _id) taxName taxCode
    _ -> do
      runDb $
        save "taxes" ["_id" =: B.ObjId (read taxId), "code" =: taxCode, "name" =: taxName]
      return  Tax {..}

removeTax :: String -> App ()
removeTax _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "taxes"


findTaxRateById :: String -> App (Maybe TaxRate)
findTaxRateById _id = do
  taxRateDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "tax_rates")
  taxRate <- pure $ getTaxRate taxRateDoc
  return taxRate

findTaxRates :: App [Maybe TaxRate]
findTaxRates = do
  taxRateDocs <- runDb $ rest =<< find (select [] "tax_rates")
  taxRates <- pure $ map getTaxRate (Just <$> taxRateDocs)
  return taxRates

getTaxRate :: Maybe Document -> Maybe TaxRate
getTaxRate document =
  case document of
     Nothing   -> Nothing
     Just doc -> TaxRate
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "code" doc
                     <*> B.lookup "description" doc
                     <*> B.lookup "group" doc
                     <*> B.lookup "tax" doc
                     <*> (pure $ doc B.!? "percentage")
                     <*> (pure $ doc B.!? "rate")
                     <*> (pure $ doc B.!?  "unit")

getTaxRateDoc :: TaxRate -> Document
getTaxRateDoc TaxRate{..} = [ "_id" =: (B.ObjId $ read taxRateId)
                            , "code" =: taxRateCode
                            , "description" =: taxRateDescription
                            , "group" =: taxRateGroup
                            , "tax" =: taxRateTax
                            , "percentage" =: taxRatePercentage
                            , "rate" =: taxRateRate
                            , "unit" =: taxRateUnit ]

insertTaxRate :: TaxRate -> App TaxRate
insertTaxRate TaxRate{..} =
  case taxRateId of
    "" -> do
      _id <- runDb $
        insert "tax_rates" ["code" =: taxRateCode, "description" =: taxRateDescription, "group" =: taxRateGroup, "tax" =: taxRateTax, "percentage" =: taxRatePercentage, "rate" =: taxRateRate, "unit" =: taxRateUnit ]
      return $ TaxRate (show _id) taxRateCode taxRateDescription taxRateGroup taxRateTax taxRatePercentage taxRateRate taxRateUnit
    _ -> do
      runDb $
        save "tax_rates" ["_id" =: B.ObjId (read taxRateId), "code" =: taxRateCode, "description" =: taxRateDescription, "group" =: taxRateGroup, "tax" =: taxRateTax, "percentage" =: taxRatePercentage, "rate" =: taxRateRate, "unit" =: taxRateUnit]
      return TaxRate {..}

removeTaxRate :: String -> App ()
removeTaxRate _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "tax_rates"
