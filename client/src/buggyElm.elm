import Mouse
import WebSocket
import Window
import Keyboard

struct = {word = ""} 
update dir table =  {table | word = "wtf"}
main = asText  <~ (foldp update struct Keyboard.arrows)
