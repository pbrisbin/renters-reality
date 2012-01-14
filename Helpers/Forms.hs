module Helpers.Forms (renderBootstrap) where

import Prelude
import Yesod

renderBootstrap :: FormRender s m a
renderBootstrap aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []

    return (res, [whamlet|
        #{fragment}
        $forall view <- views
            <div .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                <label for=#{fvId view}>#{fvLabel view}
                <div .input>
                    ^{fvInput view}
                    $maybe tt <- fvTooltip view
                        <span .help-block>#{tt}
                    $maybe err <- fvErrors view
                        <span .help-block>#{err}
        |])

    where
        has :: Maybe a -> Bool
        has (Just _) = True
        has Nothing  = False
