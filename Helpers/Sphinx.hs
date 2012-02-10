module Helpers.Sphinx
    ( executeQuery
    , buildExcerpt
    , Match(..)
    , SearchResults(..)
    ) where

import Prelude
import Yesod

import Text.Search.Sphinx
import Text.Search.Sphinx.Types
import qualified Text.Search.Sphinx.ExcerptConfiguration as E

import Control.Applicative ((<$>), (<*>))
import Data.Maybe          (catMaybes, fromMaybe)
import Data.Text           (Text)
import Text.Blaze          (preEscapedString)

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text                  as T

data SearchResults a = SearchResults
    { searchResults    :: [a]
    , searchTotal      :: Int
    , searchPage       :: Int
    , searchQuery      :: Text
    }

executeQuery :: RenderMessage m FormMessage
              => String -- ^ sphinx index
              -> Int    -- ^ sphinx port
              -> Int    -- ^ results per page
              -> (Text -> Match -> GHandler s m (Maybe a))
              -> GHandler s m (SearchResults a)
executeQuery index p per f = do
    res <- runInputGet $ (,) <$> iopt (searchField True) "q" <*> iopt intField "p"

    case res of
        -- no search term, show no results
        (Nothing, _) -> return $ SearchResults
            { searchResults    = []
            , searchTotal      = 0
            , searchPage       = 1
            , searchQuery      = ""
            }

        -- seach entered, maybe page
        (Just text, mpage) -> do
            let page = fromMaybe 1 mpage

            qres <- liftIO $ query (config p (page - 1) per) index (T.unpack text)

            (as,tot) <- case qres of
                Ok sres -> do
                    ms <- fmap catMaybes $ mapM (f text) $ matches sres
                    return (ms, total sres)

                _ -> return ([],0)

            return $ SearchResults
                { searchResults    = as
                , searchTotal      = tot
                , searchPage       = page
                , searchQuery      = text
                }

    where
        config :: Int -> Int -> Int -> Configuration
        config p' o l = defaultConfig
            { port   = p'
            , offset = o
            , limit  = l
            , mode   = Any
            }

buildExcerpt :: String -- ^ sphinx index
             -> Int    -- ^ sphinx port
             -> String -- ^ context
             -> String -- ^ search string
             -> IO Html
buildExcerpt index p context qstring = do
    excerpt <- buildExcerpts (config p) [concatMap escapeChar context] index qstring
    return $ case excerpt of
        Ok bss -> preEscapedString $ C8.unpack $ L.concat bss
        _      -> return ()

    where
        config :: Int -> E.ExcerptConfiguration
        config p' = E.altConfig { E.port = p' }

        escapeChar :: Char -> String
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar c   = [c]
