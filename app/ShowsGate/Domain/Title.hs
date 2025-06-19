{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module ShowsGate.Domain.Title (Title, mkTitle, module X) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import GHC.Records (HasField (..))

import ShowsGate.Domain.Title.Primitives as X

newtype Title = Title { _value :: Text }
instance HasField "value" Title Text where
  getField = _value

mkTitle :: Text -> Either Text Title
mkTitle txt
  | T.all isSpace txt = Left "Title cannot be empty or whitespace"
  | otherwise = Right $ Title txt

