{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root (getRootR) where

import Foundation

getRootR :: Handler RepHtml
getRootR = do
    muid <- maybeAuthId
    defaultLayout $ do
        setTitle "Home"
        addWidget $(widgetFile "root")
