{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.WebSockets
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import ClassyPrelude.Conduit (($$), forever)
import Data.Conduit.List as CL

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    webSockets chatApp
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing


--mapM_C :: (MonoFoldable c, Monad m) => (Element c -> m ()) -> Consumer c m ()
mapM_C = CL.mapM_ . Import.mapM_



chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ ("Welcome, " :: Text) <> name
    app <- getYesod
    let writeChan = chatChan app
    return ()
    {-
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan
    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChan $ name <> ": " <> msg))
    -}
