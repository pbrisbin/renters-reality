module Helpers.Review
    ( reviewForm
    , updateReview
    , insertReview
    , maybeReviewer
    , requireReviewer
    , doAndRedirect
    ) where

import Import
import Helpers.Forms
import Helpers.Model
import Yesod.Goodies
import Control.Monad (unless)
import Data.Time     (getCurrentTime)
import Database.Persist.Query.GenericSql ()

data ReviewForm = ReviewForm
    { rfIp        :: Text
    , rfLandlord  :: Text
-- FIXME
--    , rfGrade     :: Grade
    , rfTimeframe :: Text
    , rfAddress   :: Textarea
    , rfReview    :: Markdown
    }

reviewForm :: Maybe Review -> Maybe Text -> Text -> Form ReviewForm
reviewForm mr ml ip = renderBootstrap $ ReviewForm
    <$> areq hiddenField "" (Just ip)
    <*> areq textField   "Landlord"
        { fsId = Just "landlord-input" } ml

-- FIXME
--    <*> areq selectGrade "Grade"      (fmap reviewGrade     mr)
    <*> areq textField   "Time frame" (fmap reviewTimeframe mr)

    <*> areq textareaField "Address" (fmap reviewAddress mr)

    <*> areq markdownField "Review"
        { fsClass = ["review-entry"]
        } (fmap reviewContent mr)

    where
        -- FIXME
        --selectGrade :: Field Renters Renters Grade
        --selectGrade = selectField [ ("A+", Aplus), ("A", A), ("A-", Aminus)
                                  --, ("B+", Bplus), ("B", B), ("B-", Bminus)
                                  --, ("C+", Cplus), ("C", C), ("C-", Cminus)
                                  --, ("D+", Dplus), ("D", D), ("D-", Dminus)
                                  --, ("F" , F    )
                                  --]

updateReview :: ReviewId -> ReviewForm -> Handler ReviewId
updateReview rid rf = do
    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    runDB $ update rid [ ReviewLandlord  =. landlordId
                       , ReviewGrade     =. Bplus -- FIXME
                       , ReviewAddress   =. rfAddress   rf
                       , ReviewTimeframe =. rfTimeframe rf
                       , ReviewContent   =. rfReview    rf
                       ]

    return rid

insertReview :: UserId -> ReviewForm -> Handler ReviewId
insertReview uid rf = do
    now        <- liftIO getCurrentTime
    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    runDB $ insert $ Review
            { reviewCreatedDate = now
            , reviewIpAddress   = rfIp rf
            , reviewGrade       = Bplus -- FIXME
            , reviewAddress     = rfAddress rf
            , reviewContent     = rfReview rf
            , reviewTimeframe   = rfTimeframe rf
            , reviewReviewer    = uid
            , reviewLandlord    = landlordId
            }

maybeReviewer :: Document -> Handler Bool
maybeReviewer (Document _ r _ _) = do
    muid <- maybeAuth
    return $ case muid of
        Just (uid,_) -> uid == reviewReviewer r
        _            -> False

requireReviewer :: Document -> Handler ()
requireReviewer d@(Document rid r _ _) = do
    uid <- requireAuthId
    unless (uid == reviewReviewer r) $ do
        tm <- getRouteToMaster
        redirect $ tm (ReviewR rid)

-- | On sucessful FormResult, perform and action on that result which
--   returns a ReviewId, then redirect to the review with that id.
doAndRedirect :: FormResult a -> (a -> Handler ReviewId) -> Handler ()
doAndRedirect (FormSuccess res) f = do
    tm  <- getRouteToMaster
    rid <- f res

    redirect $ tm (ReviewR rid)

doAndRedirect _ _ = return ()
