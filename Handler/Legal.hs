{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Legal (getLegalR) where

import Foundation

getLegalR :: Handler RepHtml
getLegalR = defaultLayout $ do
    setTitle "Legal information"
    addWidget $(widgetFile "legal")
