module Handler.Legal where

import Import

getLegalR :: Handler RepHtml
getLegalR = defaultLayout $ do
    setTitle "Legal information"
    addWidget $(widgetFile "legal")
