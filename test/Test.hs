module Test 
  ( withClient
  , Host
  , try
  , flushDB )
where

import Control.Exception (throwIO)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Servant.Client
import Test.Hspec
import System.Process (system)
import System.Exit ( ExitCode( ExitSuccess ) )

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ 
    flip aroundWith innerSpec $ \ action manager ->
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv manager baseUrl)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
  runClientM action clientEnv

flushDB :: IO ()
flushDB = do
  ExitSuccess <- system "mm rollback"
  ExitSuccess <- system "mm"
  return ()
