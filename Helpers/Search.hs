module Helpers.Search
    ( SearchForm(..)
    , SearchResult(..)
    , executeSearch
    , matchToResult
    ) where

import Import
import Helpers.Sphinx

import Yesod.Markdown
import Database.Persist.Store (PersistValue(..))
import qualified Data.Text as T

data SearchResult = SearchResult
    { resId       :: ReviewId
    , resReview   :: Review
    , resLandlord :: Text
    , resExcerpt  :: Html
    }

matchToResult :: Text -> Match -> Handler (Maybe SearchResult)
matchToResult text match = do
    let rid = Key . PersistInt64 $ documentId match

    runDB $ do
        mreview <- get rid
        case mreview of
            Just r -> do
                mlandlord <- get (reviewLandlord r)
                case mlandlord of
                    Just l -> do
                        excerpt <- liftIO $ mkExcerpt (reviewContent r) text
                        return $ Just $ SearchResult
                                            { resId       = rid 
                                            , resReview   = r
                                            , resLandlord = (landlordName l)
                                            , resExcerpt  = excerpt
                                            }

                    _ -> return Nothing

            _ -> return Nothing

mkExcerpt :: Markdown -> Text -> IO Html
mkExcerpt (Markdown s) qstring = buildExcerpt s (T.unpack qstring)
