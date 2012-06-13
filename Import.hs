-- NOTE: I choose to export and use Data.Aeson.Types.object which has
-- different semantics than Yesod.JSON.object but allows for mixed types
-- to be the object values.
module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , (<>)
    , Text
    , module Data.Monoid
    , module Data.Aeson.Types
    , module Control.Applicative
    ) where

import Prelude hiding (writeFile, readFile)
import Yesod hiding (Route(..), setTitle, object)
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Data.Aeson.Types

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
