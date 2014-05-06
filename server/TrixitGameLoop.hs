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

import Trixit
import Utils
{-
model the game loop with a conduit that waits
for messages and emits other messages
-}

data Payload = StateUpdate GameState | Err String

data IncomingMessage = PlayerAction {sender :: PlayerID , content :: PlayerMove} | Timeout {tid :: Int} 
data OutgoingMessage = OutgoingMessage {receiver :: PlayerID, payload :: Payload}
                     | RequestTimeout {rtid :: Int}
                     | Terminate GameState

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
defaultTimeout = 30 * sec

runGame :: (Monad m, Functor m) => Int -> GameState -> Conduit IncomingMessage m OutgoingMessage
runGame roundIndex gameState  = do
  if' (waitingFor gameState /= [])
    (do
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
      forM (players finalState) (\p -> DC.yield $ OutgoingMessage (playerID p) $ StateUpdate finalState)
      runGame (roundIndex + 1) finalState
    )
    (DC.yield Terminate $ GameState)

{-
input players and communication Channel
return gameResult
-}

type AuthToken = String
data ClientChanMessage = Handshake AuthToken (TChan ServerMessage)
                        -- sent by internal thread to tell the game handler to stop listening
                        | ClientMessage IncomingMessage
                        | ClientTimeout 

data ServerMessage = AssignID PlayerID
                    | FailedAuth
                    | ServerMessage Payload

data PlayerInfo = PlayerInfo {authToken :: AuthToken, infoPlayerID :: PlayerID}


pushAfter microSec x chan = liftIO $ CC.threadDelay microSec >> atomically (writeTChan chan x)

sourceTChan chan = (liftIO $ atomically $ readTChan chan) >>= DC.yield >> sourceTChan chan

serverMessageSink readChan clients
  = do
    (Just m) <- DC.await
    case m of
      OutgoingMessage receiver payload -> do
        liftIO $ atomically $ writeTChan (fromJust $ (DHM.lookup clients receiver)) ServerMessage payload
        serverMessageSink readChan clients
      RequestTimeout index -> do
        liftIO $ forkIO $ pushAfter defaultTimeout (ClientMessage $ Timeout index) writeTChan
        serverMessageSink readChan clients
      Terminate gs -> return gs

handleGame :: (MonadIO m) => TChan ClientChanMessage -> [PlayerInfo] -> m ()
handleGame readChan playerInfos
  = do
    liftIO $ forkIO $ pushAfter (10 * sec) ClientTimeout readChan
    clients <- (P.flip iterateLoop) DHM.empty $ \clients -> do
      -- when all are gathered
      when (DHM.size clients == P.length playerInfos) $ quit clients
      clientMsg <- liftIO $ atomically $ readTChan readChan
      case clientMsg of
        Handshake auth responseChan
          -> case P.filter ( (auth == ) . authToken) playerInfos of
              [p] -> return $ DHM.insert (infoPlayerID p) responseChan clients
              [] -> liftIO $ atomically $ writeTChan responseChan FailedAuth >> return clients
        ClientTimeout -> quit clients
    sourceTChan $= DCL.map (\(ClientMessage m) -> m)
                $= (runGame 0 (initGame $ P.map infoPlayerID playerInfos)) $= serverMessageSink
    return ()

