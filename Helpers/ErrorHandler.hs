module Helpers.ErrorHandler (rentersErrorHandler) where

import Prelude
import Yesod
import qualified Data.ByteString.Char8    as S8
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Network.Wai              as W

rentersErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y ChooseRep
rentersErrorHandler NotFound = do
    path <- fmap (TE.decodeUtf8With TEE.lenientDecode . W.rawPathInfo) waiRequest

    applyLayout "Not Found"
        [hamlet|
            <div .content>
                <div .page-header>
                    <h1>Not found
                <p>#{path}
                |]

rentersErrorHandler (PermissionDenied msg) =
    applyLayout "Permission Denied"
        [hamlet|
            <div .content>
                <div .page-header>
                    <h1>Permission Denied
                <p>#{msg}
                |]

rentersErrorHandler (InvalidArgs ia) =
    applyLayout "Invalid Arguments"
        [hamlet|
            <div .content>
                <div .page-header>
                    <h1>Invalid Arguments
                <ul>
                    $forall msg <- ia
                        <li>#{msg}
                |]

rentersErrorHandler (InternalError e) =
    applyLayout "Internal Server Error"
        [hamlet|
            <div .content>
                <div .page-header>
                    <h1>Internal Server Error
                <p>#{e}
                |]

rentersErrorHandler (BadMethod m) =
    applyLayout "Bad Method"
        [hamlet|
            <div .content>
                <div .page-header>
                    <h1>Method Not Supported
                <p>Method "#{S8.unpack m}" not supported
                |]

applyLayout :: Yesod master
            => Html
            -> HtmlUrl (Route master)
            -> GHandler sub master ChooseRep
applyLayout title body = fmap chooseRep $ defaultLayout $ do
    setTitle title
    addHamlet body
