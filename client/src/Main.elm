import Mouse
import WebSocket
import Window
import Keyboard
import Char
import String
import Graphics.Input as Input
import Graphics.Input.Field (Content, noContent, field, defaultStyle, Forward, Backward)


-- MODEL

data Stage = SelectMatchingCard | Guess | ProposeWord | Idle

type CardID = String
noCardID = "none"

type Card = { cardID : CardID, faceDown: Bool , selected : Bool}
makeCard i f s = { cardID = i, faceDown = f, selected = s}

trixitTable = {handCards = [], tableCards = [], players = [],
               stage = Idle, word = ""} 

forShoTable = {handCards = [makeCard "01" False False], tableCards = [makeCard "02" False False, makeCard "03" True False], players = [],
               stage = Idle, word = ""} 

-- UPDATE

update table = {table | word <- "wtf"}

-- DISPLAY

-- INPUTS

type CardInput = Input.Input (Maybe CardID)

chosenHandCard : CardInput
chosenHandCard = Input.input Nothing


chosenTableCard : CardInput
chosenTableCard = Input.input Nothing

cardWidth = 100
cardHeight = 200


--main = let model = foldp upd foo Mouse.position in fooDisplay <~ model ~ (lift (\m -> m.bar) model) ~ (conn (lift show model))

-- DISPLAY

-main = let model = foldp update trixitTable forShoTable

display gameState = flow down [asText gameState,
                               displayCardSet gameState.tableCards chosenTableCard,
                               displayCardSet gameState.handCards chosenHandCard] --(image cardWidth cardHeight "data/img01.jpg")]

displayCardSet : [Card] -> CardInput -> Element
displayCardSet cards cardInput = flow right (map (displayCard (cardWidth, cardHeight) cardInput) cards)                               

displayCard : (Int, Int) -> CardInput -> Card -> Element
displayCard (w, h) inp card
  = let pic = image w h (if' card.faceDown "data/reverse.jpg" ("data/img" ++ card.cardID ++ ".jpg"))
    in Input.customButton inp.handle (Just card.cardID) pic pic pic
 -- Utils   

if' c x y = if c then x else y

buttonSize : number
buttonSize = 80

button : Color -> Color -> Int -> Int -> Stage -> String -> Element
button background foreground w h command name =
    let n = min w h
        btn alpha =
            layers [ color black . container w h bottomRight .
                     color background . container (w-1) (h-1) midLeft .
                     container n n middle <| txt 0.3 foreground name
                   , color (rgba 0 0 0 alpha) (spacer w h)
                   ]
    in  Input.customButton stateInput.handle command (btn 0) (btn 0.05) (btn 0.1)

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



multipleCrap x y sockInput = flow right [asText x, asText y, asText sockInput,
                                        button lightGrey black buttonSize buttonSize Idle "wtf"]



input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)