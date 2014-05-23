====== Trixit ======

implementation of a popular card game. didn't figure out the copyright details
so can't tell you which yet.

====Components======

**client**
  frp-style client. thin client relying on the server to do the whole logic

**server**
  server using yesod and websockets.
  game logic -> Trixit.hs
  game input processing and message handling-> TrixitGameLoop.hs
  game websockets side -> Handler/TrixitHandler.hs
