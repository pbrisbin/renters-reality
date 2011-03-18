{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Forms
    ( 
    -- * Standard forms 
      landlordForm
    , landlordFromForm
    , complaintForm
    , complaintFromForm

    -- * Property search
    , AddrSearch(..)
    , propertySearchForm
    , addrFromForm
    ) where

import Yesod
import BadLandlords
import Model

import Data.Time (getCurrentTime)
import Control.Applicative ((<$>),(<*>))
import Network.Wai (remoteHost)

data AddrSearch = AddrSearch
    { addrOne   :: Maybe String
    , addrTwo   :: Maybe String
    , addrCity  :: Maybe String
    , addrState :: Maybe String
    , addrZip   :: String
    }

-- | The landlord entry is a one field input, so the button is included 
--   here. Pass the route th POST to.
landlordForm :: BadLandlordsRoute -> Widget ()
landlordForm route = [$hamlet|
    <form .landlord method="post" action="@{route}">
        <table>
            <tr>
                <th>
                    <label for="landlord">Landlord name:
                <td>
                    <input .complete size=45 placeholder="Name of landlord or management company" name="landlord" required>
                <td .landlord-button>
                    <input type="submit" value="Next">
    |]

landlordFromForm :: Handler Landlord
landlordFromForm = do
    landlord <- runFormPost' $ stringInput "landlord"
    return $ Landlord landlord

complaintForm :: String -> Widget ()
complaintForm landlord = do
    ip <- lift $ return . show . remoteHost =<< waiRequest
    [$hamlet|
    <p>
        <em>all fields are required
    <form method="post" action=@{CreateR}>
        <input type=hidden name="landlord" value=#{landlord}>
        <input type=hidden name="ip" value=#{ip}>
        <table>
            <tr>
                <th>
                    <label for="name"> Your name:
                <td>
                    <input size=30 name="name" required>
            <tr>
                <th>
                    <label for="email"> Your email:
                <td>
                    <input size=30 type="email" name="email" required>

            <tr .spacer>
                <td colspan="2">&nbsp;

            <tr>
                <th>
                    <label for="addrone"> Address line 1:
                <td>
                    <input size=30 name="addrone" placeholder="248 Kelton St" required>
            <tr>
                <th>
                    <label for="addrtwo"> Address line 2:
                <td>
                    <input size=30 name="addrtwo" placeholder="Apt 1" required>
            <tr>
                <th>
                    <label for="city"> City:
                <td>
                    <input size=30 name="city" required>
            <tr>
                <th>
                    <label for="state"> State:
                <td>
                    <input size=30 name="state" required>
            <tr>
                <th>
                    <label for="zip"> Zipcode:
                <td>
                    <input size=15 name="zip" required>

            <tr .spacer>
                <td colspan="2">&nbsp;

            <tr>
                <th>
                    <label for="complaint"> Your complaint:
                <td>
                    <textarea rows=10 cols=60 name="complaint" required>

            <tr #buttons>
                <td>&nbsp;
                <td>
                    <input type="submit">
                    <input type="reset">
|]

complaintFromForm :: Handler Complaint
complaintFromForm = do
    now     <- liftIO getCurrentTime
    content <- runFormPost' $ stringInput "complaint"

    landlordId <- findOrCreate =<< landlordFromForm

    propertyId <- findOrCreate =<< 
        (runFormPost' $ Property
            <$> stringInput "addrone"
            <*> stringInput "addrtwo"
            <*> stringInput "city"
            <*> stringInput "state"
            <*> stringInput "zip")

    -- establish ownership
    _ <- findOrCreate $ Ownership propertyId landlordId

    complainerId <- findOrCreate =<<
        (runFormPost' $ Complainer
            <$> stringInput "name"
            <*> stringInput "email"
            <*> stringInput "ip")

    ref <- newRef

    return $ Complaint
        { complaintReference   = ref
        , complaintCreatedDate = now
        , complaintContent     = content
        , complaintComplainer  = complainerId
        , complaintLandlord    = landlordId
        , complaintProperty    = propertyId
        }

propertySearchForm :: Widget ()
propertySearchForm = [$hamlet|
    <form method="post" action=@{SearchR PropertyS}>
        <table>
            ^{tableRow "addrone" "Address line 1:" "248 Kelton St"}
            ^{tableRow "addrtwo" "Address line 2:" "Apt 1"        }
            ^{tableRow "city"    "City:"           ""             }
            ^{tableRow "state"   "State:"          ""             }
            <tr>
                <th>
                    <label for="zip">Zip:
                <td>
                    <input size=30 name="zip" required>

            <tr #buttons>
                <td>&nbsp;
                <td>
                    <input type=submit valud="Next">
    |]
    where
        tableRow :: String -> String -> String -> Widget ()
        tableRow name label placeholder = [$hamlet|
            <tr>
                <th>
                    <label for=#{name}> #{label}
                <td>
                    <input size=30 name=#{name} placeholder=#{placeholder}>
            |]

addrFromForm :: Handler AddrSearch
addrFromForm = runFormPost' $ AddrSearch
    <$> maybeStringInput "addrone"
    <*> maybeStringInput "addrtwo"
    <*> maybeStringInput "city"
    <*> maybeStringInput "state"
    <*> stringInput "zip"
