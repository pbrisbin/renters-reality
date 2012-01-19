module Helpers.Sphinx
    ( SearchForm(..)
    , executeSearch
    , buildExcerpt
    , Match(..)
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

import Settings (SphinxSettings(..), sphinxSettings)

data SearchForm = SearchForm
    { sfQuery :: Maybe Text
    , sfPage  :: Maybe Int
    }

searchForm :: RenderMessage m FormMessage
           => Html
           -> MForm s m (FormResult SearchForm, GWidget s m ())
searchForm = renderDivs $ SearchForm
    <$> aopt textField "q" { fsId = Just "q", fsName = Just "q" } Nothing
    <*> aopt intField  "p" { fsId = Just "p", fsName = Just "p" } Nothing

executeSearch :: RenderMessage m FormMessage
              => (Text -> Match -> GHandler s m (Maybe a))
              -> GHandler s m (([a], GWidget s m ()), FormResult SearchForm)
executeSearch f = do
    ((res, _), _) <- runFormGet searchForm

    case res of
        FormSuccess (SearchForm (Just text) (Just page)) -> do
            qres     <- liftIO $ query config { offset = page - 1, limit = per } index (T.unpack text)
            (as,tot) <- case qres of
                Ok sres -> do
                    ms <- fmap catMaybes $ mapM (f text) $ matches sres
                    return (ms, total sres)

                _ -> return ([],0)

            return ((as, buildWidget page tot),res)

        _ -> return (([], return ()),res)

    where
        config :: Configuration
        config = defaultConfig
            { port   = sphinxPort sphinxSettings
            , mode   = Any
            }

buildExcerpt :: String -> String -> IO Html
buildExcerpt context qstring = do
    excerpt <- buildExcerpts config [concatMap escapeChar context] index qstring
    return $ case excerpt of
        Ok bss -> preEscapedString $ C8.unpack $ L.concat bss
        _      -> return ()

    where
        config :: E.ExcerptConfiguration
        config = E.altConfig { E.port = sphinxPort sphinxSettings }

        escapeChar :: Char -> String
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar c   = [c]

-- | Builds the pagination widget
buildWidget :: Int -> Int -> GWidget s m ()
buildWidget page tot = do
    let pages = (\(n, r) -> n + (min r 1)) $ tot `divMod` per

    if pages <= 1
        then return ()
        else do
            let prev = [1       ..(page-1)]
            let next = [(page+1)..pages   ]

            let lim = 9 -- don't show more than nine links on either side
            let prev' = if length prev > lim then drop ((length prev) - lim) prev else prev
            let next' = if length next > lim then take lim next else next

            curParams <- lift $ fmap reqGetParams getRequest

            [whamlet|
                <div .pagination .center>
                    <ul>
                        <li .prev :null prev:.disabled>
                            ^{linkTo curParams (page - 1) "← Previous"}

                        $if (/=) prev prev'
                            <li>^{linkTo curParams 1 "1"}
                            <li>...

                        $forall p <- prev'
                            <li>^{linkTo curParams p (show p)}

                        <li .active>
                            <a href="#">#{show page}

                        $forall n <- next'
                            <li>^{linkTo curParams n (show n)}

                        $if (/=) next next'
                            <li>...
                            <li>^{linkTo curParams tot (show tot)}

                        <li .next :null next:.disabled>
                            ^{linkTo curParams (page + 1) "Next →"}

                |]

    where
        -- preserves existing params; adds/updates the passed key/value
        updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
        updateGetParam getParams (p, n) = (T.cons '?') . T.intercalate "&"
                                        . map (\(k,v) -> k `T.append` "=" `T.append` v)
                                        . (++ [(p, n)]) . filter ((/= p) . fst) $ getParams

        linkTo :: [(Text,Text)] -> Int -> String -> GWidget s m ()
        linkTo params pg txt = do
            let param = ("p", T.pack $ show pg)

            [whamlet|
                <a href="#{updateGetParam params param}">#{txt}
                |]

index :: String
index = sphinxIndex sphinxSettings

per :: Int
per = sphinxPerPage sphinxSettings
