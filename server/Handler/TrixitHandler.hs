{-# LANGUAGE DeriveGeneric #-}

module Handler.TrixitHandler where

import Prelude as P
import Data.Aeson
import Data.Aeson.Generic as DAG
import GHC.Generics
import System.Log.Logger
import Trixit
import Utils
import Data.HashMap.Lazy as DHL
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Import
import Yesod.WebSockets
import GHC.Generics
import Data.Data
import Data.Typeable
import Data.Text as DT

import Trixit
import TrixitGameLoop
import Utils 



initializeRunningGames :: (MonadIO m) => m RunningGames
initializeRunningGames = liftIO . atomically . newTVar $ DHL.empty

findGame :: (MonadIO m) => GameID -> RunningGames -> m (Maybe RunningGame)
findGame gameID games = readGames games >>= (return . (DHL.lookup gameID))

addGame :: (MonadIO m) => GameID -> RunningGame -> RunningGames -> m ()
addGame gameID game games = readGames games >>= (writeGames games) . (DHL.insert gameID game)

removeGame :: (MonadIO m) => GameID -> RunningGames -> m ()
removeGame gameID games = readGames games >>= (writeGames games) . (DHL.delete gameID)

readGames = liftIO . atomically . readTVar
writeGames games new = liftIO . atomically $ writeTVar games new

{- weird ass serverside hack used to accomodate the 
fact that the elm frontend does not work with just 1 socket (it needs one send-one receive socket)
-}
data SocketClass = Send | Receive
    deriving (Show, Eq, Generic, Data, Typeable)
data ClientAuth = ClientAuth SocketClass AuthToken GameID
    deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON PlayerMove
instance FromJSON PlayerMove
instance ToJSON SocketClass
instance FromJSON SocketClass
instance ToJSON ClientAuth
instance FromJSON ClientAuth

-- message from server to client JSON instances
instance ToJSON GameView
instance ToJSON PlayerView
instance ToJSON TableCardView
instance ToJSON GameStage
instance ToJSON Payload

initializeOpenConnections :: (MonadIO m) => m OpenConnections
initializeOpenConnections = liftIO . atomically . newTVar $ DHL.empty

trixit2SocketApp :: WebSocketsT Handler ()
trixit2SocketApp = do
  sendTextData ("Welcome to the chat server, please enter your name." :: Text)
  maybeClientAuth <- (fmap (\ s -> DAG.decode s :: Maybe ClientAuth)) receiveData
  app <- getYesod
  let openConns = openConnections app
  case maybeClientAuth of 
    Just (ClientAuth socketClass authToken gameID) ->
      case socketClass of 
        Send -> do
          receiveChan <- liftIO . atomically $ newTChan 
          conns <- liftIO . atomically . readTVar $ openConns
          liftIO . atomically . (writeTVar openConns) $ DHL.insert (gameID, authToken) receiveChan conns
          trixitApp receiveChan
        Receive -> do
          conns <- liftIO . atomically . readTVar $ openConns
          case DHL.lookup (gameID, authToken) conns of
            Just receiveChan -> handleReceives receiveChan
            Nothing -> return ()
    Nothing -> return () -- do nothing
  return ()


handleReceives receiveChan = do
  (ServerMessage m) <- readMsg receiveChan
  sendBinaryData . DAG.encode $ m

readMsg receiveChan = liftIO $ atomically $ readTChan receiveChan

trixitApp :: TChan ServerMessage -> WebSocketsT Handler ()
trixitApp serverMsgChan = undefined

