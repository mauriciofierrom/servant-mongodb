{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Bill.API where

import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import Servant

import Common.Types
import Bill.Types
import Bill.Persistence

type ClientApi =
  "clients" :> Get '[JSON] [Client] :<|>
    "client" :> Capture "clientId" String :> Get '[JSON] Client :<|>
      "client" :> ReqBody '[JSON] Client :> Post '[JSON] Client :<|>
        "client" :> Capture "clientId" String :> DeleteNoContent '[JSON] NoContent

type BillDetailApi =
  "bill-details" :> Get '[JSON] [BillDetail] :<|>
    "bill-detail" :> Capture "billDetailId" String :> Get '[JSON] BillDetail :<|>
      "bill-detail" :> ReqBody '[JSON] BillDetail :> Post '[JSON] BillDetail :<|>
        "bill-detail" :> Capture "billDetailId" String :> DeleteNoContent '[JSON] NoContent

type BillApi =
  "bills" :> Get '[JSON] [Bill] :<|>
    "bill" :> Capture "billId" String :> Get '[JSON] Bill :<|>
      "bill" :> ReqBody '[JSON] Bill :> Post '[JSON] Bill :<|>
        "bill" :> Capture "billId" String :> DeleteNoContent '[JSON] NoContent

type FullBillApi = ClientApi
              :<|> BillDetailApi
              :<|> BillApi

fullBillServer :: AppEnv -> Server FullBillApi
fullBillServer env = clientServer env
                :<|> billDetailServer env
                :<|> billServer env

clientApi :: Proxy ClientApi
clientApi = Proxy

billDetailApi :: Proxy BillDetailApi
billDetailApi = Proxy

billApi :: Proxy BillApi
billApi = Proxy

clientServer :: AppEnv -> Server ClientApi
clientServer env =
  enter (runReaderTNat env) $ getClients :<|> getClientById :<|> addClient :<|> deleteClient

getClients :: App [Client]
getClients = do
  clients <- findClients
  return $ catMaybes clients

getClientById :: String -> App Client
getClientById _id = do
  client <- findClientById _id
  case client of
    Nothing -> (lift . throwE) err404
    Just client' -> return client'

addClient :: Client -> App Client
addClient = insertClient

deleteClient :: String -> App NoContent
deleteClient _id = do
  removeClient _id
  return NoContent

billServer :: AppEnv -> Server BillApi
billServer env =
  enter (runReaderTNat env) $ getBills :<|> getBillById :<|> addBill :<|> deleteBill

getBills :: App [Bill]
getBills = do
  bills <- findBills
  return $ catMaybes bills

getBillById :: String -> App Bill
getBillById _id = do
  bill <- findBillById _id
  case bill of
    Nothing -> (lift . throwE) err404
    Just bill' -> return bill'

addBill :: Bill -> App Bill
addBill = insertBill

deleteBill :: String -> App NoContent
deleteBill _id = do
  removeBill _id
  return NoContent

billDetailServer :: AppEnv -> Server BillDetailApi
billDetailServer env =
  enter (runReaderTNat env) $ getBillDetails :<|> getBillDetailById :<|> addBillDetail :<|> deleteBillDetail

getBillDetails :: App [BillDetail]
getBillDetails = do
  billDetails <- findBillDetails
  return $ catMaybes billDetails

getBillDetailById :: String -> App BillDetail
getBillDetailById _id = do
  billDetail' <- findBillDetailById _id
  case billDetail' of
    Nothing -> (lift . throwE) err404
    Just billDetail'' -> return billDetail''

addBillDetail :: BillDetail -> App BillDetail
addBillDetail = insertBillDetail

deleteBillDetail :: String -> App NoContent
deleteBillDetail _id = do
  removeBillDetail _id
  return NoContent
