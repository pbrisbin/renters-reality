module Helpers.Forms
    ( profileForm
    , saveProfile
    , reviewForm
    , insertReview
    , updateReview
    ) where

-- TODO: selectField causes Prelude.undefined error from server. I
-- cannot figure this out...
--
-- TODO: selectList causes No Query instance at compile.

import Import

import Yesod.Goodies
import Data.Time              (getCurrentTime)
import Database.Persist.Store (Entity(..))
import qualified Data.Text as T

renderBootstrap :: FormRender sub master a
renderBootstrap aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
\#{fragment}
$forall view <- views
    <div .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
        <label for=#{fvId view}>#{fvLabel view}
        <div.input>
            ^{fvInput view}
            $maybe tt <- fvTooltip view
                <span .help-block>#{tt}
            $maybe err <- fvErrors view
                <span .help-block>#{err}
|]
    return (res, widget)

data ReviewForm = ReviewForm
    { rfIp        :: Text
    , rfLandlord  :: Text
--    , rfGrade     :: Grade
    , rfTimeframe :: Text
    , rfAddress   :: Textarea
    , rfReview    :: Markdown
    }

data ProfileForm = ProfileForm
    { pfFullname :: Maybe Text
    , pfUsername :: Maybe Text
    , pfEmail    :: Maybe Text
    }

saveProfile :: UserId -> ProfileForm -> Handler ()
saveProfile uid pf = do
    --runDB $ update uid 
        --[ UserFullname =. pfFullename pf
        --, UserUsername =. pfUsername  pf
        --, UserEmail    =. pfEmail     pf
        --]

    tm <- getRouteToMaster
    redirect $ tm ProfileR

updateReview :: ReviewId -> ReviewForm -> Handler ReviewId
updateReview rid rf = do
    ---- might've changed
    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    --runDB $ update rid [ ReviewLandlord  =. landlordId
                       --, ReviewGrade     =. rfGrade     rf
                       --, ReviewAddress   =. rfAddress   rf
                       --, ReviewTimeframe =. rfTimeframe rf
                       --, ReviewContent   =. rfReview    rf
                       --]

    -- for type consistency
    return rid

insertReview :: UserId -> ReviewForm -> Handler ReviewId
insertReview uid rf = do
    now        <- liftIO getCurrentTime
    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    runDB $ insert $ Review
            { reviewCreatedDate = now
            , reviewIpAddress   = rfIp rf
            --, reviewGrade       = rfGrade rf
            , reviewAddress     = rfAddress rf
            , reviewContent     = rfReview rf
            , reviewTimeframe   = rfTimeframe rf
            , reviewReviewer    = uid
            , reviewLandlord    = landlordId
            }

profileForm :: User -> Form ProfileForm
profileForm u = renderBootstrap $ ProfileForm
    <$> aopt textField  "Full name" (Just $ userFullname u)
    <*> aopt textField  "User name" (Just $ userUsername u)
    <*> aopt emailField "Email"
        { fsTooltip = Just "never displayed, only used to find your gravatar"
        } (Just $ userEmail u)

reviewForm :: Maybe Review -> Maybe Text -> Text -> Form ReviewForm
reviewForm mr ml ip = renderBootstrap $ ReviewForm
    <$> areq hiddenField "" (Just ip)
    <*> areq textField   "Landlord"
        { fsId = Just "landlord-input" } ml

--    <*> areq selectGrade "Grade"      (fmap reviewGrade     mr)
    <*> areq textField   "Time frame" (fmap reviewTimeframe mr)

    <*> areq textareaField "Address" (fmap reviewAddress mr)

    <*> areq markdownField "Review"
        { fsClass = ["xxlarge"]
        } (fmap reviewContent mr)

    where
        selectGrade :: Field Renters Renters Grade
        selectGrade = selectField [ ("A+", Aplus), ("A", A), ("A-", Aminus)
                                  , ("B+", Bplus), ("B", B), ("B-", Bminus)
                                  , ("C+", Cplus), ("C", C), ("C-", Cminus)
                                  , ("D+", Dplus), ("D", D), ("D-", Dminus)
                                  , ("F" , F    )
                                  ]

findOrCreate :: PersistEntity v => v -> Handler (Key (YesodPersistBackend Renters) v)
findOrCreate v = return . either entityKey id =<< runDB (insertBy v)
