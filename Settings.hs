{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( widgetFile
    , PersistConfig
    , staticRoot
    , staticDir
    , setTitle
    ) where

import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import qualified Yesod as Y
import qualified Yesod.Default.Util
import Data.Text (Text)

setTitle :: Y.Yesod m => String -> Y.GWidget s m ()
setTitle = Y.setTitle . Y.toHtml . (++) "Renters' reality | "

type PersistConfig = PostgresConf

staticDir :: FilePath
staticDir = "static"

staticRoot :: AppConfig DefaultEnv -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

widgetFile :: String -> Q Exp
#if PRODUCTION
widgetFile = Yesod.Default.Util.widgetFileProduction
#else
widgetFile = Yesod.Default.Util.widgetFileDebug
#endif
