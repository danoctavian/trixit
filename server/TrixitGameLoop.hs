module TrixitGameLoop where

import Prelude as P
import Data.Conduit as DC
import Data.Conduit.List as DCL
import Data.Maybe
import Data.List as DL

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent as CC
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.HashMap as DHM

import System.Random.Shuffle
import System.Random
--import System.Log.Logger


import Trixit
import Utils

logger = "trixit.gameLoop"
{-
model the game loop with a conduit that waits
for messages and emits other messages
-}

data Payload = StateUpdate GameState | Err String
  deriving (Show, Eq)

data IncomingMessage = PlayerAction {sender :: PlayerID , content :: PlayerMove} | Timeout {tid :: Int} 
  deriving (Show ,Eq)
data OutgoingMessage = OutgoingMessage {receiver :: PlayerID, payload :: Payload}
                     | RequestTimeout {rtid :: Int}
                     | Terminate GameState
  deriving (Show ,Eq)

awaitWithTimeout t = do
  maybeMsg <- DC.await
  case maybeMsg of
    Just (Timeout t') -> if' (t == t') (return Nothing) (awaitWithTimeout t)
    Just action -> return $ Just action
    Nothing -> return Nothing


-- microseconds
sec = 10 ^ 6
stageTimeout ProposeWord = 30 * sec
stageTimeout (Match _) = 30 * sec
stageTimeout Guess = 30 * sec
stageTimeout Idle = 30 * sec -- waiting for game master message
defaultTimeout = 3 * sec

runGame :: (Monad m, Functor m) => Int -> GameState -> Conduit IncomingMessage m OutgoingMessage
runGame roundIndex gameState  = do
  if' (waitingFor gameState /= [])
    (do
      forM (players gameState) (\p -> DC.yield $ OutgoingMessage (playerID p) $ StateUpdate gameState)
      DC.yield $ RequestTimeout roundIndex
      (afterMessages, pending) <- (P.flip iterateLoop) (gameState, waitingFor gameState) $ \(gameState, pending) -> do
        when (pending == []) (quit (gameState, pending)) -- no one left to respond...
        maybeMsg <- lift $ awaitWithTimeout roundIndex
        case maybeMsg of
          Just (PlayerAction sender move)
            -> case applyMove (sender, move) gameState of
                Right newGameState -> return (newGameState, pending DL.\\ [sender])
                Left err -> lift $ DC.yield (OutgoingMessage sender (Err err)) >> return (gameState, pending)
          Nothing -> quit (gameState, pending)

      -- send the final state after the round to everybody    
      let finalState = if' (pending /= []) (playerTimeouts pending afterMessages) afterMessages
      runGame (roundIndex + 1) finalState
    )
    (DC.yield $ Terminate gameState)

{-
input players and communication Channel
return gameResult
-}

type AuthToken = String
data ClientChanMessage = Handshake AuthToken (TChan ServerMessage)
                        -- sent by internal thread to tell the game handler to stop listening
                        | ClientMessage IncomingMessage
                        | ClientTimeout
                        deriving (Eq)

instance Show ClientChanMessage where
  show (Handshake auth c) = "Handshake " ++ auth
  show (ClientMessage m) = "ClientMessage " ++ (show m)
  show ClientTimeout = "ClientTimeout"

data ServerMessage = AssignID PlayerID
                    | FailedAuth
                    | ServerMessage Payload
                    deriving (Show ,Eq)

data PlayerInfo = PlayerInfo {authToken :: AuthToken, infoPlayerID :: PlayerID}
  deriving (Show ,Eq)


pushAfter microSec x chan = liftIO $ CC.threadDelay microSec >> atomically (writeTChan chan x)

sourceTChan chan = (liftIO $ atomically $ readTChan chan) >>= DC.yield >> sourceTChan chan

serverMessageSink readChan clients
  = do
    (Just m) <- DC.await
    case m of
      OutgoingMessage receiver payload -> do
        liftIO $ atomically $ writeTChan (fromJust $ (DHM.lookup receiver clients)) $ ServerMessage payload
        serverMessageSink readChan clients
      RequestTimeout index -> do
        liftIO $ forkIO $ pushAfter defaultTimeout (ClientMessage $ Timeout index) readChan
        -- liftIO $ P.putStrLn $ "got req for timeout " ++ (show index)
        serverMessageSink readChan clients
      Terminate gs -> return gs

handleGame :: (MonadIO m, Functor m) => TChan ClientChanMessage -> [PlayerInfo] -> [CardID] -> m ()
handleGame readChan playerInfos cards
  = do
    liftIO $ forkIO $ pushAfter (10 * sec) ClientTimeout readChan
    clients <- (P.flip iterateLoop) DHM.empty $ \clients -> do
      -- when all are gathered
      when (DHM.size clients == P.length playerInfos) $ quit clients
      clientMsg <- liftIO . atomically . readTChan $ readChan
      case clientMsg of
        Handshake auth responseChan
          -> case P.filter ( (auth == ) . authToken) playerInfos of
              [p] -> return $ DHM.insert (infoPlayerID p) responseChan clients
              [] -> liftIO $ atomically $ writeTChan responseChan FailedAuth >> return clients
        ClientTimeout -> quit clients
  --  randGen <- liftIO $ getStdGen
    let shuffledCards = shuffle' cards (P.length cards) (mkStdGen 1)
    sourceTChan readChan
                $= (DCL.filter isClientMessage) $= (DCL.map (\(ClientMessage m) -> m))
                $= (runGame 0 (initGame (P.map infoPlayerID playerInfos) shuffledCards))
                $$ (serverMessageSink readChan clients)
    return ()

isClientMessage (ClientMessage _) = True
isClientMessage _ = False

printer chan = (liftIO $ atomically $ readTChan chan) >>= (liftIO . P.putStrLn) >> (printer chan)
writeLog :: (MonadIO m) => TChan String -> String -> m ()
writeLog chan s = liftIO $ atomically $ writeTChan chan s

manualTestHandleGame :: (MonadIO m, Functor m) => m ()
manualTestHandleGame = do
  let playerCount = 3
      cards = P.take 40  $ P.map show $ [1..]
  --liftIO $ updateGlobalLogger TrixitGameLoop.logger (setLevel DEBUG)
  logChan <- liftIO $ atomically $ newTChan
  liftIO $ forkIO $ printer logChan

  playerChans <- replicateM playerCount (liftIO $ atomically $ newTChan)
  readChan <- liftIO $ atomically $ newTChan
  let playerInfos = P.take playerCount $ P.map (\i -> let p = pname i in PlayerInfo p p) [1..]
  
  forM (P.zip playerInfos playerChans)
       (\(PlayerInfo auth plID, chan) -> do
        liftIO $ atomically $ writeTChan chan (AssignID plID)
        liftIO $ forkIO $ do
          liftIO $ atomically $ writeTChan readChan (Handshake auth chan)
          m <- liftIO $ atomically $ readTChan chan
          case m of 
            AssignID _ -> return () -- it's ok
            FailedAuth -> liftIO $ writeLog logChan "ERROR; client not authenticated"
            other -> liftIO $ writeLog logChan $ "wtf is this " ++ (show other)
          forever $ do
            (ServerMessage payload) <- liftIO $ atomically $ readTChan chan
            liftIO $ writeLog logChan $ "Player " ++ plID ++ " received " ++ (show payload)
        )
  liftIO $ writeLog logChan "let the games begin!"
  writeLog logChan "++++++++++++++++++++"



  liftIO $ forkIO $ handleGame readChan playerInfos cards

  send readChan 1 (Proposal "jack" "18")
  liftIO $ threadDelay (round $ (fromIntegral defaultTimeout) * (2.5 :: Float))
  send readChan 2 (Vote "18")

  liftIO $ threadDelay (defaultTimeout * 300)
  return ()

pname i = "P" ++ (show i)
send readChan i move
  = liftIO $ atomically $ writeTChan readChan $ ClientMessage $ PlayerAction (pname i) move