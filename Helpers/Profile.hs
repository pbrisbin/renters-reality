module Helpers.Profile
    ( profileForm
    , saveProfile
    ) where

import Import

data ProfileForm = ProfileForm
    { pfFullname :: Maybe Text
    , pfUsername :: Maybe Text
    , pfEmail    :: Maybe Text
    }

profileForm :: User -> Form ProfileForm
profileForm u = renderBootstrap $ ProfileForm
    <$> aopt textField  "Full name" (Just $ userFullname u)
    <*> aopt textField  "User name" (Just $ userUsername u)
    <*> aopt emailField "Email"
        { fsTooltip = Just "never displayed, only used to find your gravatar"
        } (Just $ userEmail u)

saveProfile :: UserId -> ProfileForm -> Handler ()
saveProfile uid pf = do
    runDB $ update uid 
        [ UserFullname =. pfFullname pf
        , UserUsername =. pfUsername pf
        , UserEmail    =. pfEmail    pf
        ]

    redirect ProfileR
