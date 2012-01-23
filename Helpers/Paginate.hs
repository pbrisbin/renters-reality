module Helpers.Paginate (paginate) where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T

-- | A widget showing pagination links. Follows bootstrap principles.
--   Utilizes a \"p\" GET param but leaves all other GET params intact.
paginate :: Int -- ^ current page
         -> Int -- ^ items per page
         -> Int -- ^ total number of items
         -> GWidget s m ()
paginate page per tot = do
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
