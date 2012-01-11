module Handler.Review
    ( getReviewR
    , postReviewR
    , getEditR
    , postEditR
    , getNewR
    , postNewR
    ) where

import Import
import Helpers.Forms

import Yesod.Goodies
import Control.Monad          (unless)
import Database.Persist.Store (PersistValue(PersistText, PersistInt64))
import Network.Wai            (remoteHost)
import qualified Data.Text as T

getReviewR :: ReviewId -> Handler RepHtml
getReviewR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> do
            reviewTime <- liftIO . humanReadableTime . reviewCreatedDate $ review d
            ownReview  <- isReviewer d
            defaultLayout $ do
                setTitle "View review"
                addWidget $(widgetFile "review/show")

        Nothing -> notFound

    where
        isReviewer :: Document -> Handler Bool
        isReviewer (Document _ r _ _) = do
            muid <- maybeAuth
            return $ case muid of
                Just (uid,_) -> uid == reviewReviewer r
                _            -> False

        rText :: ReviewId -> Text
        rText = go . unKey

            where
                go (PersistText  t) = t
                go (PersistInt64 i) = T.pack $ show i
                go _                = ""

getEditR :: ReviewId -> Handler RepHtml
getEditR rid = do
    (uid, _) <- requireAuth
    docs     <- siteDocs =<< getYesod

    case docByReviewId rid docs of
        Just d@(Document _ r l _) -> do
            -- not your review, redirect to the view page
            unless (uid == reviewReviewer r) $ do
                tm <- getRouteToMaster
                redirect $ tm (ReviewR rid)

            ip <- return . T.pack . show . remoteHost =<< waiRequest
            ((res, form), enctype) <- runFormPost $ reviewForm (Just r) (Just $ landlordName l) ip

            case res of
                FormSuccess rf -> do
                    return ()
                    tm  <- getRouteToMaster
                    _   <- updateReview rid rf
                    redirect $ tm (ReviewR rid)

                _ -> return ()

            defaultLayout $ do
                setTitle "Edit review"
                addWidget $(widgetFile "review/edit")

        _ -> notFound

getNewR :: Handler RepHtml
getNewR = do
    (uid, _) <- requireAuth
    ml       <- lookupGetParam "landlord"

    ip <- return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- runFormPost $ reviewForm Nothing ml ip

    case res of
        FormSuccess rf -> do
            return ()
            tm  <- getRouteToMaster
            rid <- insertReview uid rf
            redirect $ tm (ReviewR rid)

        _ -> return ()

    defaultLayout $ do
        setTitle "New review"
        addWidget $(widgetFile "review/new")

postReviewR :: ReviewId -> Handler RepHtml
postReviewR = getReviewR

postNewR :: Handler RepHtml
postNewR = getNewR

postEditR :: ReviewId -> Handler RepHtml
postEditR = getEditR
