{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ShowsGate.Contracts.Movies
  ( MoviesAPI
  , AddMovie, GetMovie, GetAllMovies, DeleteMovie
  , MovieNotFound(..)
  , MovieResp(..)
  , AddMovieReqBody(..)
  ) where

import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics (Generic)
import Servant

import ShowsGate.Contracts.ValidationErrorResp (ValidationErrorResp)

type MoviesAPI
  =    AddMovie
  :<|> GetMovie
  :<|> GetAllMovies
  :<|> DeleteMovie

data MovieNotFound = MovieNotFound

instance HasStatus MovieNotFound where
  type StatusOf MovieNotFound = 404

instance ToJSON MovieNotFound where
  toJSON _ = "movie not found"

data MovieResp = MovieResp
  { titleId :: UUID
  , title :: Text
  , runtimeMinutes :: Maybe Int
  , releaseYear :: Maybe Int
  } deriving (Generic, ToJSON)

data AddMovieReqBody = AddMovieReqBody
  { title :: Text
  , runtimeMinutes :: Maybe Int
  , releaseYear :: Maybe Int
  } deriving (Generic, FromJSON)

type AddMovie
  =  ReqBody '[JSON] AddMovieReqBody
  :> UVerb 'POST '[JSON] '[WithStatus 201 MovieResp, ValidationErrorResp]

type GetMovie
  =  Capture "titleId" UUID
  :> UVerb 'GET '[JSON] '[WithStatus 200 MovieResp, MovieNotFound]

type GetAllMovies = Get '[JSON] [MovieResp]

type DeleteMovie = Capture "titleId" UUID :> DeleteNoContent

