module Helpers.Sphinx
    ( SearchForm(..)
    , executeQuery
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
import Data.Maybe          (catMaybes)
import Data.Text           (Text)
import Text.Blaze          (preEscapedString)

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text                  as T

data SearchForm = SearchForm
    { sfQuery :: Maybe Text
    , sfPage  :: Maybe Int
    }

data SearchResults a = SearchResults
    { searchResults    :: [a]
    , searchTotal      :: Int
    , searchPage       :: Int
    , searchQuery      :: Text
    , searchFormResult :: FormResult SearchForm
    }

searchForm :: RenderMessage m FormMessage
           => Html
           -> MForm s m (FormResult SearchForm, GWidget s m ())
searchForm = renderDivs $ SearchForm
    <$> aopt textField "q" { fsId = Just "q", fsName = Just "q" } Nothing
    <*> aopt intField  "p" { fsId = Just "p", fsName = Just "p" } Nothing

executeQuery :: RenderMessage m FormMessage
              => String -- ^ sphinx index
              -> Int    -- ^ sphinx port
              -> Int    -- ^ results per page
              -> (Text -> Match -> GHandler s m (Maybe a))
              -> GHandler s m (SearchResults a)
executeQuery index p per f = do
    ((res, _), _) <- runFormGet searchForm

    case res of
        FormSuccess (SearchForm (Just text) (Just page)) -> do
            qres     <- liftIO $ query (config p (page - 1) per) index (T.unpack text)
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
                , searchFormResult = res
                }

        _ -> return $ SearchResults
                { searchResults    = []
                , searchTotal      = 0
                , searchPage       = curPage  res
                , searchQuery      = curQuery res
                , searchFormResult = res
                }

    where
        config :: Int -> Int -> Int -> Configuration
        config p' o l = defaultConfig
            { port   = p'
            , offset = o
            , limit  = l
            , mode   = Any
            }

        curQuery :: FormResult SearchForm -> Text
        curQuery (FormSuccess (SearchForm (Just q) _)) = q
        curQuery _                                     = ""

        curPage :: FormResult SearchForm -> Int
        curPage (FormSuccess (SearchForm _ (Just p'))) = p'
        curPage _                                      = 1

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
