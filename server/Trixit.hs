{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}

module Trixit where

import Prelude as P
import Data.List as DL
import Data.Maybe
import Data.List.Split as DLS
import "mtl" Control.Monad.Error
import Utils
import GHC.Generics
import Data.Data
import Data.Typeable

type CardID = String
type PlayerID = String
type Word = String
data Player = Player {playerID :: PlayerID, playerScore :: Int, handCards :: [CardID]}
  deriving (Show, Eq)
data GameStage =  ProposeWord | Match Word | Guess | Idle
  deriving (Show, Eq, Generic, Data, Typeable)

type PlayerVote = PlayerID
-- cardID, owner , votes and faceUp
data TableCard = TableCard {tcID :: CardID, tcOwner :: PlayerID, tcVotes :: [PlayerVote], tcFaceUp :: Bool}
  deriving (Show, Eq)

data GameState = GameState  {
                               tableCards :: [TableCard]
                             , cardDeck :: [CardID]
                             , players :: [Player]
                             , stage :: GameStage
                             , proposer :: PlayerID -- index of proposer in the array
                           }
                 | EndGame {gameWinners :: [Player]}
                 deriving (Show, Eq)

data TableCardView = TableCardView {tcvCardID :: CardID, tcvVotes :: [PlayerVote], tcvOwner :: Maybe PlayerID}
  deriving (Show, Eq, Generic, Data, Typeable)
data PlayerView = PlayerView {pvID :: PlayerID, pvScore :: Int}
  deriving (Show, Eq, Generic, Data, Typeable)
data GameView = GameView {
                        gvTableCards :: [TableCardView]
                      , gvPlayerHand :: [CardID]
                      , gvPlayers :: [PlayerView]
                      , gvStage :: GameStage
                      , gvProposer :: PlayerID
                    }
                deriving (Show, Eq, Generic, Data, Typeable)

faceDownID = "0"
playerView :: GameState -> PlayerID -> GameView
playerView gs pid
  = GameView {
        gvTableCards = P.map (\tc -> TableCardView (if' (tcFaceUp tc) (tcID tc) faceDownID) (tcVotes tc)
                                     (if' (stage gs == Idle) (Just $ tcOwner tc) Nothing) ) $ tableCards gs
      , gvPlayerHand = handCards $ findPlayer pid (players gs)
      , gvPlayers = P.map (\p -> PlayerView (playerID p) (playerScore p)) (players gs)
      , gvStage = stage gs
      , gvProposer = proposer gs
    }

-- StartRound just progresses the game from Idle to the next round
data PlayerMove = Proposal Word CardID | Vote CardID | MatchWord CardID | StartRound
  deriving (Show, Eq, Generic, Data, Typeable)

gameMaster :: PlayerID
gameMaster = "0"

initGame :: [PlayerID] -> [CardID] -> GameState
initGame players deck
  = dealCards 5 $ GameState {tableCards = [], cardDeck = deck,
              stage = ProposeWord, proposer = players !! 0,
              players = P.map (\pid -> Player pid 0 []) players} 


applyMove :: (PlayerID, PlayerMove) -> GameState -> Either String GameState
applyMove move state
  = case ((stage state), move) of
    (ProposeWord, (player, Proposal word cardID))
      -> if' (proposer state == player && owns player cardID state)
         (Right $ putDown player cardID $ state {stage =  Match word})
         (Left $ "wrong player proposing")
    (Match word, (player, MatchWord cardID))
      -> if' (not (P.elem player $ P.map tcOwner $ tableCards state)
              && (owns player cardID state))
          (Right $ (progressToGuess $ putDown player cardID state))
          (Left "wrong player match")
    (Guess, (player, Vote cardID))
      -> if' ((P.elem cardID $ P.map tcID $ tableCards state)
              && (not $ P.elem player $ votes $ tableCards state) && player /= (proposer state))
         (Right $ (\g -> if' (allVoted g) (progressToEndRound g) g) $ state {tableCards =
            update ((cardID ==) . tcID) (\ tc -> tc {tcVotes = player : tcVotes tc}) (tableCards state)})
         (Left "wrong vote")
    (Idle, (gameMaster, StartRound)) -> Right $ newRound state    
    _ -> Left "player input provided at the wrong stage"

votes = (P.concatMap tcVotes)

owns player cardID state = P.elem cardID $ handCards $ findPlayer player (players state)
-- assumes correct input
putDown player cardID state
  = state {players = update ((player ==) . playerID)
              (\p -> p {handCards = handCards p \\ [cardID]})
              (players state),
          tableCards =  (TableCard cardID player [] False) : tableCards state}

progressToGuess state
  = if' ((P.length $ tableCards state) == (P.length $ players state))
    (state {stage = Guess, tableCards = (P.map (\c -> c {tcFaceUp = True}) $ tableCards state)})
    state 


allVoted state = (P.length $ votes $ tableCards state) == (P.length $ players state) - 1

progressToEndRound state
  =  state
    {players = updateScores (score (tableCards state) (proposer state)) $ players state,
    stage = Idle
    } 

nextProposer state =  after (proposer state) (P.map playerID $ players state)

newRound state
  = dealCards 1 $ state {
    tableCards = [],
    cardDeck = cardDeck state ++ (P.map tcID $ tableCards state),
    proposer = nextProposer state,
    stage = ProposeWord
  }

toWinningState state
  = let winners = P.filter ((winnerScore<=) . playerScore) $ players state in
    if' (winners /= []) (EndGame winners) state

winnerScore = 30
winScore = 3 -- when no unanimity, the propser and electors of his card get this
decoyScore = 1 -- whne somebody picks your card
lossScore = 0 -- when you lose
unanimityScore = 2 -- when all or nobody guesses, all except the propopser get this

score :: [TableCard] -> PlayerID  -> [(PlayerID, Int)]
score tableCards proposer
  | P.elem goodVotes [[], votes tableCards]
      = (proposer, lossScore) : (P.zip others $ P.repeat unanimityScore)
  | otherwise = (proposer, winScore) : (P.map (\p -> (p, othersScore p))  others)
    where 
      goodVotes = tcVotes $ cardOf proposer
      cardOf p = (P.filter ((p == ) . tcOwner) tableCards) !! 0
      others = P.map tcOwner tableCards \\ [proposer]
      othersScore p = (if' (P.elem p goodVotes) winScore lossScore)
                      + (P.length $ tcVotes $ cardOf p)

-- deal n cards to each 
dealCards n state 
  = state {players = P.map (\(p, hand) -> p {handCards = handCards p ++ hand})
                     $ P.zip (players state) hands, cardDeck = rest}
    where
      hands = DLS.chunksOf n dealt
      (dealt, rest) = P.splitAt (P.length (players state) * n) $ cardDeck state 

updateScores scores players
  = P.map (\p -> p {playerScore = playerScore p + (fromJust $ P.lookup (playerID p) scores)}) players

update :: (a -> Bool) -> (a -> a) -> [a] -> [a]
update updateable change = P.map (\x -> if' (updateable x) (change x) x) 

-- assumes player exists
findPlayer :: PlayerID -> [Player] -> Player
findPlayer pid ps = head $ P.filter ((pid == ) . playerID) ps

after :: (Eq a) => a -> [a] -> a
after x xs = P.dropWhile (x /=) (xs ++ xs) !! 1

others s = filter ((proposer s /= ) . playerID) $ players s

waitingFor :: GameState -> [PlayerID]
waitingFor game =
  case (stage game) of
    ProposeWord -> [proposer game]
    Match w -> playerIDs \\ (P.map tcOwner $ tableCards game)
    Guess -> playerIDs \\ (proposer game : (votes $ tableCards game))
    Idle -> [gameMaster]
  where
    playerIDs = P.map playerID $ players game





{-
timeout handling:
Propose word -> skip to next person
Match -> select first card in the hand
vote -> ignore players who didn't vote 
-}

{- a default decision for the players in case they time out
cannot fail, timeouts are always considered correct with respect to the state
-}
playerTimeouts :: [PlayerID] -> GameState -> GameState
playerTimeouts latePlayers game
  = case (stage game) of
      ProposeWord -> game {proposer = nextProposer game}
      (Match w) -> foldl (\game player -> fromRight $ applyMove (player,
            MatchWord (head $ handCards $ findPlayer player $ players game)) game) game latePlayers
      Guess -> progressToEndRound game 
      Idle -> error "there is no action on the player side for the idle stage" -- nope...

-- so it kinda works knowing this :)
mTest1 = let bs =initGame (P.map show $ P.take 5 [1..]) (P.map show $ P.take 40 [1..]) in
          foldl (>>=) (return $ bs)
          
          [\s -> applyMove (proposer s, Proposal "shit" ( head $ handCards $ findPlayer (proposer s) $ players $ s)) s
          , (\s -> foldl (>>=) (return s) (P.map
            (\p -> applyMove (playerID p, MatchWord (handCards p !! 0))) $ others s))
          , (\s -> foldl (>>=) (return s) (P.map
            (\p -> applyMove (playerID p, Vote (tcID $ tableCards s !! 0))) $ others s))
          , gmSaysNext
          ]

mTest2 = let bs =initGame (P.map show $ P.take 5 [1..]) (P.map show $ P.take 40 [1..]) in
          foldl (>>=) (return $ bs)
          
          [\s -> applyMove (proposer s, Proposal "shit" ( head $ handCards $ findPlayer (proposer s) $ players $ s)) s
          , othersTimeout
          , othersTimeout
          , gmSaysNext
          ]


othersTimeout s = return $ playerTimeouts ( (\\ [proposer s]) $ P.map playerID $ players s) s
gmSaysNext = applyMove (gameMaster, StartRound)
mTestScore1 = score [TableCard "1" "P1" ["P2"] True, TableCard "2" "P2" ["P3"] True,
                    TableCard "3" "P3" ["P4"] True, TableCard "4" "P4" [] True] "P1"