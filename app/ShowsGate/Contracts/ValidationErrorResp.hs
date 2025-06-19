{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module ShowsGate.Contracts.ValidationErrorResp (ValidationErrorResp(..)) where

import Data.Aeson
import Data.Text
import Servant

import Data.List.NonEmpty
import GHC.Generics

data ValidationErrorResp = ValidationErrorResp
  { message :: Text
  , errors :: NonEmpty Text
  } deriving (Generic, ToJSON)

instance HasStatus ValidationErrorResp where
  type StatusOf ValidationErrorResp = 422

