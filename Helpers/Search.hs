module Helpers.Search
    ( SearchResult(..)
    , Search(..)
    , search
    , search_
    ) where

import Data.List  (sortBy)
import Data.Ord   (comparing)
import Data.Maybe (catMaybes)

import qualified Data.Text as T

-- | A ranked search result
data SearchResult a = SearchResult
    { searchRank   :: Double
    , searchResult :: a
    }

class Search a where
    -- | Artifically adjust the ranking of certain values by providing a 
    --   factor by which to multiply the natural rank. This could be 
    --   used to f.e. rank more recent items higher without having the 
    --   code that into the @'match'@ function itself.
    factor :: a -> Double
    factor _ = 1

    -- | Given a search term and some @a@, provide @Just@ a ranked 
    --   result or @Nothing@.
    match :: T.Text -> a -> Maybe (SearchResult a)

-- | Excute a search on a list of @a@s, apply the @factor@ and rank the 
--   results by @searchRank@ descending.
search :: Search a => T.Text -> [a] -> [SearchResult a]
search t = rankResults . map applyFactor . catMaybes . map (match t)

-- | Identical to search but discards the ranking.
search_ :: Search a => T.Text -> [a] -> [a]
search_ t = map searchResult . search t

applyFactor :: Search a => SearchResult a -> SearchResult a
applyFactor (SearchResult d v) = SearchResult (d * factor v) v

rankResults :: [SearchResult a] -> [SearchResult a]
rankResults = reverse . sortBy (comparing searchRank)
