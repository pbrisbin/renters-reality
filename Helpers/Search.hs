module Helpers.Search where

import Import
import Yesod.Markdown

import Data.Maybe             (catMaybes)
import Database.Persist.Store (PersistValue(..))
import Text.Blaze             (preEscapedString)
import Text.Search.Sphinx
import Text.Search.Sphinx.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import qualified Text.Search.Sphinx.ExcerptConfiguration as E

import System.IO
debugPrint :: String -> Handler ()
debugPrint = liftIO . hPutStrLn stderr

data SearchForm = SearchForm
    { sfQuery :: Maybe Text
    , sfPage  :: Maybe Int
    }

-- this form is never actually rendered, just used to parse the GET
-- params when the search page is loaded
searchForm :: Form SearchForm
searchForm = renderDivs $ SearchForm
    <$> aopt textField "q" { fsId = Just "q", fsName = Just "q" } Nothing
    <*> aopt intField  "p" { fsId = Just "p", fsName = Just "p" } Nothing

data SearchResult = SearchResult
    { resId       :: ReviewId
    , resReview   :: Review
    , resLandlord :: Text
    , resExcerpt  :: Html
    }

searchReviews :: FormResult SearchForm -> Handler ([SearchResult], Widget)
searchReviews (FormSuccess (SearchForm (Just text) _)) = do
    res   <- liftIO $ query defaultConfig { port = 9312, mode = Any } "renters-idx" (T.unpack text)
    sress <- case res of
        Ok sres -> fmap catMaybes $ mapM (matchToSearchResult text) $ matches sres
        _       -> return []
            
    return (sress,return ())

searchReviews _ = return ([],return())

matchToSearchResult :: Text -> Match -> Handler (Maybe SearchResult)
matchToSearchResult text (Match docId _ attrs) = do
    let rid = Key $ PersistInt64 docId

    runDB $ do
        mreview <- get rid
        case mreview of
            Just r -> do
                mlandlord <- get (reviewLandlord r)
                case mlandlord of
                    Just l -> do
                        excerpt <- liftIO $ buildExcerpt r text
                        return . Just $ SearchResult rid r (landlordName l) excerpt

                    _ -> return Nothing

            _ -> return Nothing

buildExcerpt :: Review -> Text -> IO Html
buildExcerpt r text = do
    excerpt <- buildExcerpts E.altConfig { E.port = 9312 } [escape $ reviewContent r] "renters-idx" (T.unpack text)
    return $ case excerpt of
        Ok bss -> preEscapedString $ C8.unpack $ L.concat bss
        _      -> return ()

escape :: Markdown -> String
escape (Markdown s) = concatMap escapeChar s

    where
        escapeChar :: Char -> String
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar c   = [c]
