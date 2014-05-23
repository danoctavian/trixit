import Mouse
import WebSocket
import Window
import Keyboard
import Char
import String
import Graphics.Input as Input
import Graphics.Input.Field as Field
import Signal
import Dict

{-
  FRONTEND implementation of a popular card game. can't tell you which cause i didn't think how to 
  deal with copyright yet :)

  4 main sections:
    ** MODEL - all data types representing the state in the game
    ** INPUTS - player inputs (eg. proposed word, chosen card)
    ** DISPLAY - how to draw the game 
    ** UPDATE - how to change the state of the game based on player inputs/server data

    main peculiarity:
      server communication needs to be done with 2 sockets mainly because elm websocket library
      is not flexible eonugh to let you use the output signal of a websocket to define the input
      signal of that socket. 
      another solution: use js hacks 
      long-term solution: change the library and  switch to one-socket communincation
-}

-- MODEL

data Stage = Match | Guess | ProposeWord | Idle

type CardID = String
noCardID = "none"

type Card = { cardID : CardID, faceDown: Bool , selected : Bool}
makeCard i f s = { cardID = i, faceDown = f, selected = s}

type Player = String

type TrixitTable = {handCards : [Card], tableCards : [Card], players : [Player], stage : Stage, word : String}

type Decision = {completed : Bool, stage : Stage, finalValue : [GameInput], {- the decision signal value -}
                stageInputs : StageInputs}
type StageInputs = {required : [InputType], acquired : Dict.Dict String GameInput}
-- initial value; no real meaning
initDecision : Decision
initDecision = {completed = False, stage = Idle, finalValue = [None], stageInputs = {required = [], acquired = Dict.empty}}

trixitTable = {handCards = [], tableCards = [], players = [],
               stage = Idle, word = ""}


-- filled up state used for manual visual debugging
-- TODO: remove when done with
forShoTable = {handCards = [makeCard "03" False False, makeCard "01" False False], tableCards = [makeCard "02" False False, makeCard "04" False False], players = [],
               stage = Idle, word = ""} 



-- INPUTS

type CardInput = Input.Input CardID


chosenHandCard : CardInput
chosenHandCard = Input.input "n/a"

chosenTableCard : CardInput
chosenTableCard = Input.input "n/a"

submitCommand : Input.Input GameInput
submitCommand = Input.input None

wordInput : Input.Input Field.Content
wordInput = Input.input Field.noContent

submitWord : Input.Input Bool
submitWord = Input.input True


stageInputHack : Input.Input GameInput
stageInputHack = Input.input <| StageInput {stage = Idle}

cardWidth = 100
cardHeight = 200


data GameInput =  ChosenHand CardID | ChosenDeck CardID | Word String | StageInput {stage : Stage} | Submit | None
data InputType = ChosenHandInput | ChosenDeckInput | WordInput


--UPDATE

update tableUpdate table = table

inputType : GameInput -> InputType
inputType input
  = case input of
    (ChosenHand _) -> ChosenHandInput
    (ChosenDeck _) -> ChosenDeckInput
    (Word _) -> WordInput
 --   (StageInput _) -> "StageInput" 

isCard : GameInput -> Bool
isCard = (elem [ChosenDeckInput, ChosenHandInput]) . inputType
getCardID gi
  = case gi of
    (ChosenHand cid) -> cid
    (ChosenDeck cid) -> cid

-- = let model = foldp upd foo Mouse.position in fooDisplay <~ model ~ (lift (\m -> m.bar) model) ~ (conn (lift show model))

requiredInputs stage
  = case stage of
      Idle -> []
      Guess -> [ChosenDeckInput]
      Match -> [ChosenHandInput]
      ProposeWord -> [ChosenHandInput, WordInput]

processDecision : GameInput -> Decision -> Decision
processDecision gameInput decision
  = case gameInput of
      StageInput {stage} -> {decision | stage <- stage, completed <- False,
                                        stageInputs <- {required = requiredInputs stage, acquired = Dict.empty}}
      Submit -> finalizeDecision decision                                  
      other -> let collectedInputs = acceptInput decision.stageInputs other
                in {decision | stageInputs <- collectedInputs, completed <- areComplete collectedInputs}

areComplete stageInputs = dictSize stageInputs.acquired == length stageInputs.required

acceptInput : StageInputs -> GameInput -> StageInputs
acceptInput stageInputs input = if
  | any (\t -> (inputType input) == t) stageInputs.required -> 
      {stageInputs | acquired <- Dict.insert (show <| inputType input) input stageInputs.acquired}
  | otherwise -> stageInputs -- no change

finalizeDecision : Decision -> Decision
finalizeDecision d = if' d.completed {d | finalValue <- Dict.values d.stageInputs.acquired} d


-- DISPLAY

-- combine the signals in the same data type ? with "data" to be able to process them in any interleaving
--main = let model = foldp update forShoTable (merges [constant forShoTable, ]) in display <~ model

display : TrixitTable -> Field.Content -> Decision -> Element
display gameState sentence decision
  = let sel = map getCardID <| filter isCard <| Dict.values decision.stageInputs.acquired
        markS = markSelected sel 
    in flow down
      [asText gameState
      , asText sel
      , Field.field Field.defaultStyle wordInput.handle id "write sentence" sentence
      , button lightGrey black buttonSize buttonSize submitCommand.handle Submit "Submit"
      , displayCardSet (markS gameState.tableCards) chosenTableCard
      , displayCardSet (markS gameState.handCards) chosenHandCard]

markSelected : [CardID] -> [Card] -> [Card]
markSelected selected = map (\c -> if' (elem selected c.cardID) {c | selected <- True} c)

displayCardSet : [Card] -> CardInput -> Element
displayCardSet cards cardInput = flow right (map (displayCard (cardWidth, cardHeight) cardInput) cards)                               

displayCard : (Int, Int) -> CardInput -> Card -> Element
displayCard (w, h) inp card
  = let pic = layers <|
    [color (rgba 255 0 255 0.5) (spacer w  h)
    , opacity  (if' card.selected 0.5 1) <|
    image w h (if' card.faceDown "data/reverse.jpg" ("data/img" ++ card.cardID ++ ".jpg"))]
          
                      
    in Input.customButton inp.handle card.cardID pic pic pic


displayDecision decision
  = flow down <| [
      asText decision
      , flow right [ 
        button lightGrey black buttonSize buttonSize stageInputHack.handle (StageInput {stage = Guess}) "GUESS stage"
      ]
    ]

decisionShower
  = let decision =
        (foldp processDecision initDecision <| merges
          [stageInputHack.signal, submitCommand.signal, lift ChosenHand chosenHandCard.signal,
           lift ChosenDeck chosenTableCard.signal, lift (\w -> Word w.string) wordInput.signal])
        gameState = foldp update forShoTable (constant forShoTable)

    in (\decision word gameState ->
        flow down <| [asText "DEBUG", displayDecision decision , asText "ENDDEBUG", display gameState word decision] )
        <~ decision ~ wordInput.signal ~ gameState

 -- Utils   
-- function if
if' c x y = if c then x else y

elem xs y = any (\x -> y == x) xs

-- dictionary length
dictSize = length . Dict.toList 


buttonSize : number
buttonSize = 80

button : Color -> Color -> Int -> Int -> Input.Handle a -> a -> String -> Element
button background foreground w h handle value name =
    let n = min w h
        btn alpha =
            layers [ color black . container w h bottomRight .
                     color background . container (w-1) (h-1) midLeft .
                     container n n middle <| txt 0.3 foreground name
                   , color (rgba 0 0 0 alpha) (spacer w h)
                   ]
    in  Input.customButton handle value (btn 0) (btn 0.05) (btn 0.1)

txt : Float -> Color -> String -> Element
txt p clr string =
    leftAligned . Text.height (p * buttonSize) .
    typeface ["Helvetica Neue","Sans-serif"] . Text.color clr <| toText string




-- small websocket two -way communication snippet
-- TODO: replace with real main function
conn = WebSocket.connect "ws://localhost:8010"
echoSocket = asText <~ conn (conn (constant "Hello WebSocket!"))

main = asText <~ (WebSocket.connect "ws://localhost:3000" (constant "whowtf"))


