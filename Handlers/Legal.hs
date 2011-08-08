{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Legal (getLegalR) where

import Renters

getLegalR :: Handler RepHtml
getLegalR = defaultLayout $ do
    setTitle "Legal information"
    addWidget $(widgetFile "legal")
