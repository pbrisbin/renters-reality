{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Forms where -- TODO

import Renters
import Model
import Yesod
import Yesod.Goodies.Markdown
import Control.Applicative ((<$>),(<*>))
import Data.Monoid         (mempty)
import Data.Time           (getCurrentTime)
import qualified Data.Text as T

data ReviewForm = ReviewForm
    { rfIp        :: T.Text
    , rfLandlord  :: T.Text
    , rfAddress   :: Textarea
    , rfTimeframe :: T.Text
    , rfGrade     :: Grade
    , rfReview    :: Markdown
    }

data MarkdownExample = MarkdownExample
    { mdText :: String
    , mdHtml :: Widget ()
    }

reviewForm :: Maybe Review -- ^ for use in edit
           -> Maybe T.Text -- ^ maybe landlord name (for use in new)
           -> T.Text       -- ^ IP address of submitter
           -> FormMonad (FormResult ReviewForm, Widget())
reviewForm mr ml ip = do
    (fIp       , fiIp       ) <- hiddenField    (ffs ""            "ip"       ) $ Just ip
    (fLandlord , fiLandlord ) <- stringField    (ffs "Landlord:"   "landlord" ) $ ml
    (fAddress  , fiAddress  ) <- textareaField  (ffs "Address:"    "address"  ) $ fmap reviewAddress   mr
    (fTimeframe, fiTimeframe) <- stringField    (ffs "Time frame:" "timeframe") $ fmap reviewTimeframe mr
    (fGrade    , fiGrade    ) <- selectField'   (ffs "Grade:"      "grade"    ) $ fmap reviewGrade     mr
    (fReview   , fiReview   ) <- markdownField  (ffs "Review:"     "review"   ) $ fmap reviewContent   mr

    return (ReviewForm 
        <$> fIp      <*> fLandlord  
        <*> fAddress <*> fTimeframe  
        <*> fGrade <*> fReview, [hamlet|
            <table .review-form>
                <tr #ip>^{fieldCell 4 fiIp}

                <tr #landlord-grade>
                    ^{fieldCell 1 fiLandlord}
                    ^{fieldCell 1 fiGrade}

                <tr #timeframe>
                    ^{fieldCell 4 fiTimeframe}
                    <td colspan=3>&nbsp;

                <tr #address>
                    ^{fieldCell 4 fiAddress}
                    <td colspan=3>&nbsp;

                <tr #review-help>
                    <td>&nbsp;
                    <td colspan="5">
                        <small>
                            <em>
                                Reviews are parsed as pandoc-style markdown. 
                                <a #open-help href="#">Tips.

                <tr #review>^{fieldCell 4 fiReview}

                <tr>
                    <td>&nbsp;
                    <td .buttons colspan="4">
                        <input type="submit" value="Save">
            |])

        where
            selectField' = selectField gradesList
                where
                    gradesList :: [(Grade, T.Text)]
                    gradesList = [ (Aplus , "A+")
                                 , (A     , "A" )
                                 , (Aminus, "A-")
                                 , (Bplus , "B+")
                                 , (B     , "B" )
                                 , (Bminus, "B-")
                                 , (Cplus , "C+")
                                 , (C     , "C" )
                                 , (Cminus, "C-")
                                 , (Dplus , "D+")
                                 , (D     , "D" )
                                 , (Dminus, "D-")
                                 , (F     , "F" )
                                 ]

            ffs :: T.Text -> T.Text -> FormFieldSettings
            ffs label theId = FormFieldSettings label mempty (Just theId) Nothing

            -- span for the input cell only
            fieldCell :: Int -> FieldInfo s m -> GWidget s m ()
            fieldCell colspan fi = [hamlet|
                <th>
                    <label for="#{fiIdent fi}">#{fiLabel fi}
                <td ##{fiIdent fi} colspan=#{show colspan}>^{fiInput fi}
                <td>
                    $maybe error <- fiErrors fi
                        #{error}
                    $nothing
                        &nbsp;
                |]

insertFromForm :: UserId -> ReviewForm -> Handler ReviewId
insertFromForm uid rf = do
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

updateFromForm :: ReviewId -> UserId -> ReviewForm -> Handler ReviewId
updateFromForm rid _ rf = do
    -- might've changed
    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    runDB $ update rid [ ReviewLandlord landlordId
                       , ReviewGrade     $ rfGrade    rf
                       , ReviewAddress   $ rfAddress  rf
                       , ReviewTimeframe $ rfTimeframe rf
                       , ReviewContent   $ rfReview   rf
                       ]

    -- for type consistency
    return rid

-- | Find or create an entity, returning its key in both cases
findOrCreate :: PersistEntity a => a -> Handler (Key a)
findOrCreate v = return . either fst id =<< runDB (insertBy v)

-- the markdown help tips
helpBoxContents :: Widget ()
helpBoxContents = [hamlet|
        <h3>Some quick examples:

        $forall mdExample <- mdExamples
            <p .example>
                <code>#{mdText mdExample} 
                will render as ^{mdHtml mdExample}

        <p>
            <em>
                Additional documentation can be found 
                <a href="http://daringfireball.net/projects/markdown/syntax">here
                \.
    |]

mdExamples :: [MarkdownExample]
mdExamples = [ MarkdownExample "*italic text*"
                    [hamlet|<em>italic text|]

             , MarkdownExample "**bold text**"
                    [hamlet|<strong>bold text|]

             , MarkdownExample "[some link](http://example.com \"link title\")"
                    [hamlet|<a href="http://example.com" title="link title">some link|]

             , MarkdownExample "![even images](http://pbrisbin.com/static/images/feed.png)"
                    [hamlet|<img alt="even images" src="http://pbrisbin.com/static/images/feed.png">|]
             ]
