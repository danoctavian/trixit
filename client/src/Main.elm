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

-- MODEL

data Stage = Match | Guess | ProposeWord | Idle

type CardID = String
noCardID = "none"

type Card = { cardID : CardID, faceDown: Bool , selected : Bool}
makeCard i f s = { cardID = i, faceDown = f, selected = s}

type Player = String

type TrixitTable = {handCards : [CardID], tableCards : [CardID], players : [Player], stage : Stage, word : String}

type Decision = {complete : Bool, stage : Stage, finalValue : [GameInput], {- the decision signal value -}
                stageInputs : {required : [InputType], acquired : Dict.Dict String GameInput}}

trixitTable = {handCards = [], tableCards = [], players = [],
               stage = Idle, word = ""}


forShoTable = {handCards = [makeCard "01" False False], tableCards = [makeCard "02" False False, makeCard "03" True False], players = [],
               stage = Idle, word = ""} 

-- UPDATE

update tableUpdate table = table--{table | word <- "wtf"}

-- DISPLAY

-- INPUTS

type CardInput = Input.Input (Maybe CardID)


chosenHandCard : CardInput
chosenHandCard = Input.input Nothing


chosenTableCard : CardInput
chosenTableCard = Input.input Nothing

sentenceInput : Input.Input Field.Content
sentenceInput = Input.input Field.noContent

sentenceInput2 : Input.Input Field.Content
sentenceInput2 = Input.input Field.noContent

submitActions : Input.Input String
submitActions = Input.input ""

toggle : Input.Input Bool
toggle = Input.input True

cardWidth = 100
cardHeight = 200


data GameInput =  ChosenHand CardID | ChosenDeck CardID | Word String | StageInput {stage : Stage} | Submit
data InputType = ChosenHandInput | ChosenDeckInput | WordInput

inputType : GameInput -> InputType
inputType input
  = case input of
    (ChosenHand _) -> ChosenHandInput
    (ChosenDeck _) -> ChosenDeckInput
    (Word _) -> WordInput
 --   (StageInput _) -> "StageInput" 


--main = let model = foldp upd foo Mouse.position in fooDisplay <~ model ~ (lift (\m -> m.bar) model) ~ (conn (lift show model))




main = displayFooInput <~ (sampleOn submitActions.signal (combine [sentenceInput.signal, sentenceInput2.signal]))
                                    ~ sentenceInput.signal ~ sentenceInput2.signal ~ toggle.signal


displayFooInput txt sentence sentence2 isVisible = flow down <| [asText <|  map (.string) txt] ++
                                (if isVisible then
                                [
                                Field.field Field.defaultStyle sentenceInput.handle id "write sentence" sentence
                                , Field.field Field.defaultStyle sentenceInput2.handle id "write sentence" sentence2
                                ] else []) ++
                                [button lightGrey black buttonSize buttonSize submitActions.handle "Wtf" "Submit"
                                , button lightGrey black buttonSize buttonSize toggle.handle (not isVisible) "toggle"
                                ]

requiredInputs stage
  = case stage of
      Idle -> []
      Guess -> [ChosenDeckInput]
      Match -> [ChosenHandInput]
      ProposeWord -> [ChosenHandInput, WordInput]


processDecision gameInput decision
  = case gameInput of
      StageInput {stage} -> {decision | stage <- stage, completed <- False,
                                        stageInputs <- {required = requiredInputs stage, acquired = Dict.empty}}
      Submit -> finalizeDecision decision                                  
      other -> let collectedInputs = acceptInput decision.stageInputs other
                in {decision | stageInputs <- collectedInputs, completed <- areComplete collectedInputs}

areComplete stageInputs = dictSize stageInputs.acquired == length stageInputs.required

acceptInput stageInputs input = if
  | any (\t -> (inputType input) == t) stageInputs.required -> 
      {stageInputs | acquired <- Dict.insert (show <| inputType input) input stageInputs.acquired}
  | otherwise -> stageInputs -- no change

finalizeDecision d = if' d.complete {d | finalValue <- Dict.toList d.stageInputs.acquired} d



-- DISPLAY

-- combine the signals in the same data type ? with "data" to be able to process them in any interleaving
--main = let model = foldp update forShoTable (merges [constant forShoTable, ]) in display <~ model


display gameState = flow down [asText gameState,
                               displayCardSet gameState.tableCards chosenTableCard,
                               displayCardSet gameState.handCards chosenHandCard]

displayCardSet : [Card] -> CardInput -> Element
displayCardSet cards cardInput = flow right (map (displayCard (cardWidth, cardHeight) cardInput) cards)                               

displayCard : (Int, Int) -> CardInput -> Card -> Element
displayCard (w, h) inp card
  = let pic = image w h (if' card.faceDown "data/reverse.jpg" ("data/img" ++ card.cardID ++ ".jpg"))
    in Input.customButton inp.handle (Just card.cardID) pic pic pic
 -- Utils   
-- function if
if' c x y = if c then x else y

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


-- FUCKAROUNDS

stateInput : Input.Input Stage
stateInput = Input.input Idle

numbers : Input.Input Int
numbers = Input.input 42
{-}

main = asText <~ conn (conn (constant "Hello WebSocket!"))
-}
--main = let s = stateInput in multipleCrap <~ Keyboard.arrows ~ Mouse.position ~  (conn (lift show (dropRepeats s.signal)))

{-
foo = {bar = (0, 0)}
main = let model = foldp upd foo Mouse.position in fooDisplay <~ model ~ (lift (\m -> m.bar) model) ~ (conn (lift show model))
fooDisplay m t connection = flow right [asText m , asText t, asText connection]
upd mouse f = {f | bar <- mouse}
-}
conn = WebSocket.connect "ws://localhost:8010"



-- data Periferals = M (Int, Int) | K (Int, Int)
--main = asText <~  (merges [(lift M Mouse.position), (lift (\e -> K (e.x, e.y)) Keyboard.arrows)])


input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)


