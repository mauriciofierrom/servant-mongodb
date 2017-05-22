{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Bill.Persistence where

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
import Bill.Types
import Item.Persistence ( getItem
                        , getItemDoc )
import Enterprise.Persistence ( getIdentificationType
                              , getIdentificationTypeDoc )

findClientById :: String -> App (Maybe Client)
findClientById _id = do
  clientDoc <- runDb $ findOne (select ["_id" =: (B.ObjId $ read _id)] "clients")
  client <- pure $ getClient clientDoc
  return client

findClients :: App [Maybe Client]
findClients = do
  clientDocs <- runDb $ rest =<< find (select [] "clients")
  clients <- pure $ map getClient (Just <$> clientDocs)
  return clients

getClient :: Maybe Document -> Maybe Client
getClient document =
  case document of
     Nothing   -> Nothing
     Just doc  -> Client
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "name" doc
                     <*> B.lookup "last_name" doc
                     <*> getIdentificationType (B.lookup "identification_type" doc)
                     <*> B.lookup "identification" doc
                     <*> B.lookup "address" doc
                     <*> B.lookup "phone" doc
                     <*> B.lookup "email" doc

getClientDoc :: Client -> Document
getClientDoc Client{..} = [ "_id" =: B.ObjId (read clientId)
                          , "name" =: clientName
                          , "last_name" =: clientLastName
                          , "identification_type" =: getIdentificationTypeDoc clientIdentificationType
                          , "identification" =: clientIdentification
                          , "address" =: clientAddress
                          , "phone" =: clientPhone
                          , "email" =: clientEmail ]

-- this code is not secure
-- what happens if the insert failed?
-- Maybe pattern match with Failure type
insertClient :: Client -> App Client
insertClient Client{..} =
  case clientId of
    "" -> do
      _id <- runDb $
        insert "clients" [ "name" =: clientName
                         , "last_name" =: clientLastName
                         , "identification_type" =: getIdentificationTypeDoc clientIdentificationType
                         , "identification" =: clientIdentification
                         , "address" =: clientAddress
                         , "phone" =: clientPhone
                         , "email" =: clientEmail ]
      return $ Client (show _id) clientName clientLastName clientIdentificationType clientIdentification clientAddress clientPhone clientEmail
    _ -> do
      runDb $
        save "clients" [ "_id" =: B.ObjId (read clientId)
                       , "name" =: clientName
                       , "last_name" =: clientLastName
                       , "identification_type" =: getIdentificationTypeDoc clientIdentificationType
                       , "identification" =: clientIdentification
                       , "address" =: clientAddress
                       , "phone" =: clientPhone
                       , "email" =: clientEmail ]
      return Client {..}

removeClient :: String -> App ()
removeClient _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "clients"

findBillDetailById :: String -> App (Maybe BillDetail)
findBillDetailById _id = do
  bdDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "bill_details")
  billDetail <- pure $ getBillDetail bdDoc
  return billDetail

findBillDetails :: App [Maybe BillDetail]
findBillDetails = do
  bdDocs <- runDb $ rest =<< find (select [] "bill_details")
  billDetails <- pure $ map getBillDetail (Just <$> bdDocs)
  return billDetails

getBillDetail :: Maybe Document -> Maybe BillDetail
getBillDetail document =
  case document of
     Nothing   -> Nothing
     Just doc  -> BillDetail
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> getItem (B.lookup "item" doc)
                     <*> B.lookup "quantity" doc

getBillDetailDoc :: BillDetail -> Document
getBillDetailDoc BillDetail{..} = [ "_id" =: B.ObjId (read bdId)
                                  , "item" =: getItemDoc bdItem
                                  , "quantity" =: bdQuantity]

insertBillDetail :: BillDetail -> App BillDetail
insertBillDetail BillDetail{..} =
  case bdId of
    "" -> do
      _id <- runDb $
        insert "bill_details" ["item" =: getItemDoc bdItem, "quantity" =: bdQuantity ]
      return $ BillDetail (show _id) bdItem bdQuantity
    _ -> do
      runDb $
        save "bill_details" ["_id" =: B.ObjId (read bdId), "item" =: getItemDoc bdItem, "quantity" =: bdQuantity ]
      return $ BillDetail bdId bdItem bdQuantity

removeBillDetail :: String -> App ()
removeBillDetail _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "bill_details"

findBillById :: String -> App (Maybe Bill)
findBillById _id = do
  billDoc <- runDb $ findOne (select ["_id" =: (B.ObjId $ read _id)] "bills")
  bill <- pure $ getBill billDoc
  return bill

findBills :: App [Maybe Bill]
findBills = do
  billDocs <- runDb $ rest =<< find (select [] "bills")
  bills <- pure $ map getBill (Just <$> billDocs)
  return bills

getBill :: Maybe Document -> Maybe Bill
getBill document =
  case document of
    Nothing -> Nothing
    Just doc -> Bill <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "number" doc
                     <*> B.lookup "date" doc
                     <*> getClient (B.lookup "client" doc)
                     <*> mapM getBillDetail (B.lookup "bill_details" doc)

-- this code is not secure
-- what happens if the insert failed?
-- Maybe pattern match with Failure type
insertBill :: Bill -> App Bill
insertBill Bill{..} =
  case billId of
    "" -> do
      _id <- runDb $
        insert "bills" [ "number" =: billNumber
                       , "date" =: billDate
                       , "client" =: getClientDoc billClient
                       , "bill_details" =: map getBillDetailDoc billDetail ]
      return $ Bill (show _id) billNumber billDate billClient billDetail
    _ -> do
      runDb $
        save "bills" [ "_id" =: B.ObjId (read billId)
                     , "number" =: billNumber
                     , "date" =: billDate
                     , "client" =: getClientDoc billClient
                     , "bill_details" =: map getBillDetailDoc billDetail ]
      return Bill {..}

removeBill :: String -> App ()
removeBill _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "bills"
