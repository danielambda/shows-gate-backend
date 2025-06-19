{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module ShowsGate.WebAPI.Movies (MonadUUID(..), moviesServer) where

import Servant
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Validation (Validation (Failure, Success), validationNel, validation)
import Data.Functor ((<&>))
import Database.Beam.Postgres (Postgres)
import Database.Beam
import DatabaseModels (TitleType(..), ShowsGateDB(..), showsGateDB, MovieT (movieTitleId))
import qualified DatabaseModels as DB
import Data.List.NonEmpty (NonEmpty)
import ShowsGate.Contracts.Movies (MoviesAPI, MovieResp (..), AddMovie, GetAllMovies, DeleteMovie, AddMovieReqBody(..))
import ShowsGate.Contracts.ValidationErrorResp (ValidationErrorResp(..))
import ShowsGate.Domain.Movie (Movie (..))
import ShowsGate.Domain.Title (TitleId(..), mkTitle)
import ShowsGate.Domain.Movie.Primitives (mkMovieRuntime)
import ShowsGate.WebAPI.Movies.Get (getMovie)
import ShowsGate.DB (MonadPgConn, runPg)

moviesServer :: (MonadPgConn m, MonadUUID m) => ServerT MoviesAPI m
moviesServer
  =    addMovie
  :<|> getMovie
  :<|> getAllMovies
  :<|> deleteMovie

class Monad m => MonadUUID m where
  newUUID :: m UUID

movieToResp :: Movie -> MovieResp
movieToResp movie = MovieResp
  { titleId = movie.titleId.value
  , title = movie.title.value
  , runtimeMinutes = movie.mRuntimeMinutes <&> (.value)
  , releaseYear = fromIntegral <$> movie.mReleaseYear
  }

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

selectList
  :: (MonadPgConn m, FromBackendRow Postgres (QExprToIdentity res), Projectible Postgres res)
  => Q Postgres db QBaseScope res -> m [QExprToIdentity res]
selectList query = runPg $ runSelectReturningList $ select query

movieFromDB :: (DB.Title, DB.Movie) -> Validation (NonEmpty Text) Movie
movieFromDB (dbTitle, dbMovie) = do
  let titleId = TitleId dbTitle.titleId
  title <- validationNel $ mkTitle dbTitle.title
  mRuntimeMinutes <- traverse
    (validationNel . mkMovieRuntime . fromIntegral)
    dbMovie.movieRuntimeMinutes
  let mReleaseYear = fromIntegral <$> dbMovie.movieReleaseYear
  return Movie{..}

getAllMovies :: MonadPgConn m => ServerT GetAllMovies m
getAllMovies = map movieToResp <$> do
  results <- selectList $ do
    movie <- all_ showsGateDB.movies
    title <- all_ showsGateDB.titles
    guard_ $ movie.movieTitleId `references_` title
    return (title, movie)
  return $ concatMap (validation (const []) (:[]) . movieFromDB) results

deleteMovie :: MonadPgConn m => ServerT DeleteMovie m
deleteMovie titleId = do
  runPg $ runDelete $ delete showsGateDB.titles (\title -> title.titleId ==. val_ titleId)
  return NoContent
