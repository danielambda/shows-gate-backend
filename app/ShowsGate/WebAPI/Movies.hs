module ShowsGate.WebAPI.Movies (moviesServer) where

import Servant (ServerT, type (:<|>) (..), NoContent (NoContent))
import Data.Text (Text)
import Data.Validation (Validation, validationNel, validation)
import Database.Beam.Postgres (Postgres)
import Database.Beam (FromBackendRow, QExprToIdentity, Projectible, QBaseScope, Q, runSelectReturningList, select, all_, references_, guard_, runDelete, delete, SqlEq ((==.)), SqlValable (val_))

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)

import ShowsGate.DB (MonadPgConn, runPg)
import ShowsGate.DB.Models (ShowsGateDB(..), showsGateDB, MovieT (movieTitleId))
import qualified ShowsGate.DB.Models as DB
import ShowsGate.Domain.Movie (Movie (..))
import ShowsGate.Domain.Title (TitleId(..), mkTitle)
import ShowsGate.Domain.Movie.Primitives (mkMovieRuntime)
import ShowsGate.Contracts.Movies (MoviesAPI, MovieResp (..), GetAllMovies, DeleteMovie)
import ShowsGate.WebAPI.Movies.Get (getMovie)
import ShowsGate.WebAPI.Common.MonadUUID (MonadUUID)
import ShowsGate.WebAPI.Movies.Add (addMovie)

moviesServer :: (MonadPgConn m, MonadUUID m) => ServerT MoviesAPI m
moviesServer
  =    addMovie
  :<|> getMovie
  :<|> getAllMovies
  :<|> deleteMovie

movieToResp :: Movie -> MovieResp
movieToResp movie = MovieResp
  { titleId = movie.titleId.value
  , title = movie.title.value
  , runtimeMinutes = movie.mRuntimeMinutes <&> (.value)
  , releaseYear = fromIntegral <$> movie.mReleaseYear
  }

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
