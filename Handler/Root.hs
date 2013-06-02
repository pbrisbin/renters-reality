module Handler.Root where

import Import
import Yesod.Auth

getRootR :: Handler RepHtml
getRootR = do
    muid   <- maybeAuthId
    mauthR <- fmap authRoute getYesod
    defaultLayout $ do
        setTitle "Home"
        $(widgetFile "root")
