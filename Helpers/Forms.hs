module Helpers.Forms
    ( runReviewFormNew
    , runReviewFormEdit
    , runProfileFormPost
    , profileEditForm
    ) where

import Import

import Yesod.Goodies
import Data.Time              (getCurrentTime)
import Database.Persist.Store (Entity(..))
import Network.Wai            (remoteHost)
import qualified Data.Text as T

-- | Render a form using Bootstrap-friendly HTML syntax.
renderBootstrap :: FormRender sub master a
renderBootstrap aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
#{fragment}
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
    , rfGrade     :: Grade
    , rfTimeframe :: Text
    , rfAddress   :: Textarea
    , rfReview    :: Markdown
    }

data ProfileEditForm = ProfileEditForm
    { eFullname :: Maybe Text
    , eUsername :: Maybe Text
    , eEmail    :: Maybe Text
    }

runProfileFormPost :: Handler ()
runProfileFormPost = do
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormPost $ profileEditForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    where
        saveChanges :: UserId -> ProfileEditForm -> Handler ()
        saveChanges uid ef = do
            return ()
            --runDB $ update uid 
                --[ UserFullname =. eFullname ef
                --, UserUsername =. eUsername ef
                --, UserEmail    =. eEmail    ef
                --]

            --tm <- getRouteToMaster
            --redirect $ tm ProfileR

runReviewFormEdit :: Document -> Widget
runReviewFormEdit (Document rid r l _) = do
    ip <- lift $ return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormPost $ reviewForm (Just r) (Just $ landlordName l) ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            _   <- updateFromForm rf
            redirect $ tm (ReviewsR rid)

    [whamlet|<form enctype="#{enctype}" method="post">^{form}|]

    where
        updateFromForm :: ReviewForm -> Handler ReviewId
        updateFromForm  rf = do
            -- might've changed
            landlordId <- findOrCreate $ Landlord $ rfLandlord rf

            --runDB $ update rid [ ReviewLandlord  =. landlordId
                               --, ReviewGrade     =. rfGrade     rf
                               --, ReviewAddress   =. rfAddress   rf
                               --, ReviewTimeframe =. rfTimeframe rf
                               --, ReviewContent   =. rfReview    rf
                               --]

            -- for type consistency
            return rid

runReviewFormNew :: UserId -> Maybe T.Text -> Widget
runReviewFormNew uid ml = do
    ip <- lift $ return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormPost $ reviewForm Nothing ml ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            rid <- insertFromForm rf
            redirect $ tm (ReviewsR rid)

    [whamlet|<form enctype="#{enctype}" method="post">^{form}|]

    where
        insertFromForm :: ReviewForm -> Handler ReviewId
        insertFromForm rf = do
            now        <- liftIO getCurrentTime
            landlordId <- findOrCreate $ Landlord $ rfLandlord rf

            runDB $ insert $ Review
                    { reviewCreatedDate = now
                    , reviewIpAddress   = rfIp rf
                    , reviewGrade       = rfGrade rf
                    , reviewAddress     = rfAddress rf
                    , reviewContent     = rfReview rf
                    , reviewTimeframe   = rfTimeframe rf
                    , reviewReviewer    = uid
                    , reviewLandlord    = landlordId
                    }

profileEditForm :: User -> Form ProfileEditForm
profileEditForm u = renderBootstrap $ ProfileEditForm
    <$> aopt textField  "Full name" (Just $ userFullname u)
    <*> aopt textField  "User name" (Just $ userUsername u)
    <*> aopt emailField "Email"
        { fsTooltip = Just "never displayed, only used to find your gravatar"
        } (Just $ userEmail u)

reviewForm :: Maybe Review -> Maybe Text -> Text -> Form ReviewForm
reviewForm mr ml ip = renderBootstrap $ ReviewForm
    <$> areq hiddenField "Ip" (Just ip)
    <*> areq textField   "Landlord"
        { fsId = Just "search-landlord" } ml

    <*> areq selectGrade "Grade"      (fmap reviewGrade     mr)
    <*> areq textField   "Time frame" (fmap reviewTimeframe mr)

    <*> areq textareaField "Address"
        { fsClass = ["address-box"]
        } (fmap reviewAddress mr)

    <*> areq markdownField "Review"
        { fsClass = ["review-box"]
        } (fmap reviewContent mr)

    where
        selectGrade = selectField [ ("A+", Aplus), ("A", A), ("A-", Aminus)
                                  , ("B+", Bplus), ("B", B), ("B-", Bminus)
                                  , ("C+", Cplus), ("C", C), ("C-", Cminus)
                                  , ("D+", Dplus), ("D", D), ("D-", Dminus)
                                  , ("F" , F    )
                                  ]

findOrCreate :: PersistEntity v => v -> Handler (Key (YesodPersistBackend Renters) v)
findOrCreate v = return . either entityKey id =<< runDB (insertBy v)
