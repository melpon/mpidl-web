{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Yesod                                  as Y
import qualified Yesod.Form                             as YForm
import qualified Data.Text                              as T

import Foundation (Handler, Form, Route(HomeR))
import Settings (widgetFile)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Y.Html
getHomeR = do
    (formWidget, formEnctype) <- YForm.generateFormPost sampleForm
    let submission = Nothing :: Maybe (Y.FileInfo, T.Text)
        handlerName = "getHomeR" :: T.Text
    Y.defaultLayout $ do
        aDomId <- Y.newIdent
        Y.setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Y.Html
postHomeR = do
    ((result, formWidget), formEnctype) <- YForm.runFormPost sampleForm
    let handlerName = "postHomeR" :: T.Text
        submission = case result of
            YForm.FormSuccess res -> Just res
            _ -> Nothing

    Y.defaultLayout $ do
        aDomId <- Y.newIdent
        Y.setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (Y.FileInfo, T.Text)
sampleForm = YForm.renderDivs $ (,)
    <$> YForm.fileAFormReq "Choose a file"
    <*> YForm.areq YForm.textField "What's on the file?" Nothing
