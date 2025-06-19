{-# LANGUAGE OverloadedStrings #-}

module ShowsGate.WebAPI.Movies.Add (addMovie) where

import Database.Beam
import Data.Validation (Validation(Failure, Success), validationNel)
import Servant (ServerT, WithStatus (WithStatus), respond)

import Data.Functor ((<&>))

import ShowsGate.Contracts.Movies (AddMovie, AddMovieReqBody(..), MovieResp (..))
import ShowsGate.Contracts.ValidationErrorResp (ValidationErrorResp(..))
import ShowsGate.DB (MonadPgConn, runPg)
import ShowsGate.DB.Models (TitleType (MovieType), showsGateDB)
import qualified ShowsGate.DB.Models as DB
import ShowsGate.Domain.Movie (Movie(..))
import ShowsGate.Domain.Movie.Primitives (mkMovieRuntime)
import ShowsGate.Domain.Title (TitleId(..), mkTitle)
import ShowsGate.WebAPI.Common.MonadUUID (MonadUUID (..))

movieToDb :: Movie -> (DB.Title, DB.Movie)
movieToDb movie = (dbTitle, dbMovie)
  where
    dbTitle = DB.Title
      { titleId = movie.titleId.value
      , title = movie.title.value
      , titleType = MovieType
      }
    dbMovie = DB.Movie
      { movieTitleId = pk dbTitle
      , movieReleaseYear = fromIntegral <$> movie.mReleaseYear
      , movieRuntimeMinutes = fromIntegral . (.value) <$> movie.mRuntimeMinutes
      }

addMovie :: (MonadPgConn m, MonadUUID m) => ServerT AddMovie m
addMovie reqBody = do
  titleId <- TitleId <$> newUUID
  case validMovie titleId of
    Failure validationErrors ->
      respond $ ValidationErrorResp "invalid movie" validationErrors
    Success movie -> do
      runPg $ do
        let (dbTitle, dbMovie) = movieToDb movie
        runInsert $ insert showsGateDB.titles $ insertValues [dbTitle]
        runInsert $ insert showsGateDB.movies $ insertValues [dbMovie]
      respond $ WithStatus @201 $ movieToResp movie
  where
    validMovie titleId = do
      title <- validationNel $ mkTitle reqBody.title
      runtimeMinutes <- case reqBody.runtimeMinutes of
        Nothing -> pure Nothing
        Just rm -> validationNel $ Just <$> mkMovieRuntime rm
      let year = fromIntegral <$> reqBody.releaseYear
      return $ Movie titleId title runtimeMinutes year

movieToResp :: Movie -> MovieResp
movieToResp movie = MovieResp
  { titleId = movie.titleId.value
  , title = movie.title.value
  , runtimeMinutes = movie.mRuntimeMinutes <&> (.value)
  , releaseYear = fromIntegral <$> movie.mReleaseYear
  }

