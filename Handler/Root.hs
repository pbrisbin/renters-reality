module Handler.Root where

import Import

getRootR :: Handler RepHtml
getRootR = do
    muid   <- maybeAuthId
    mauthR <- fmap authRoute getYesod
    defaultLayout $ do
        setTitle "Home"
        addWidget $(widgetFile "root")

searchWidget :: Widget
searchWidget = $(widgetFile "root/_search")

tipsWidget :: Widget
tipsWidget = $(widgetFile "root/_tips")
