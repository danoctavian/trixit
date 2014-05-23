import Mouse
import WebSocket
import Window
import Keyboard

{-
  ELM BUILD BUG- elm-server hangs when trying to compile this code
  reported to the owner already
-}

struct = {word = ""} 
update dir table =  {table | word = "wtf"}
main = asText  <~ (foldp update struct Keyboard.arrows)
