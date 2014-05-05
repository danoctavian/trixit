{-# LANGUAGE PackageImports #-}

module Trixit where

import Prelude as P
import Data.List as DL
import Data.Maybe
import Data.List.Split as DLS
import "mtl" Control.Monad.Error

type CardID = String
type PlayerID = String
type Word = String
data Player = Player {playerID :: PlayerID, playerScore :: Int, handCards :: [CardID]}
  deriving (Show, Eq)
data GameStage =  ProposeWord | Match Word | Guess | Idle
  deriving (Show, Eq)

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

-- StartRound just progresses the game from Idle to the next round
data PlayerMove = Proposal Word CardID | Vote CardID | MatchWord CardID | StartRound

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
         (Right $ progressToEndRound $ state {tableCards =
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

progressToEndRound state
  = if' ((P.length $ votes $ tableCards state) == (P.length $ players state) - 1) 
    (state
    {players = updateScores (score (tableCards state) (proposer state)) $ players state,
    stage = Idle
    })
    state

newRound state
  = dealCards 1 $ state {
    tableCards = [],
    cardDeck = cardDeck state ++ (P.map tcID $ tableCards state),
    proposer = after (proposer state) (P.map playerID $ players state),
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
findPlayer  :: PlayerID -> [Player] -> Player
findPlayer pid ps = (P.filter ((pid == ) . playerID) ps) !! 0

after :: (Eq a) => a -> [a] -> a
after x xs = P.dropWhile (x /=) (xs ++ xs) !! 1

if' c a b = if c then a else b


others s = filter ((proposer s /= ) . playerID) $ players s

-- so it kinda works knowing this :)
mTest1 = let bs =initGame (P.map show $ P.take 5 [1..]) (P.map show $ P.take 40 [1..]) in
          foldl (>>=) (return $ bs)
          
          [\s -> applyMove (proposer s, Proposal "shit" ( head $ handCards $ findPlayer (proposer s) $ players $ s)) s
          , (\s -> foldl (>>=) (return s) (P.map
            (\p -> applyMove (playerID p, MatchWord (handCards p !! 0))) $ others s))
          , (\s -> foldl (>>=) (return s) (P.map
            (\p -> applyMove (playerID p, Vote (tcID $ tableCards s !! 0))) $ others s))
          , (applyMove (gameMaster, StartRound))
          ]