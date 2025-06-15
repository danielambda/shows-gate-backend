{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}

module ShowsGate.WebAPI.Movies (MonadUUID(..), MonadPgConn(..), MoviesAPI, moviesServer, AddMovieReqBody(..), MovieResp(..)) where

import Servant
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Time (Year)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Validation (Validation (Failure, Success), validationNel)
import Data.Functor ((<&>))
import Database.Beam.Postgres (runBeamPostgres, Connection, Postgres, Pg)
import Database.Beam
import DatabaseModels (TitleType(..), ShowsGateDB(..), showsGateDB, MovieT (movieTitleId))
import qualified DatabaseModels as DB
import Data.Aeson (FromJSON, ToJSON)

newtype TitleId = TitleId { value :: UUID }

newtype Title = Title { value :: Text }
mkTitle :: Text -> Either Text Title
mkTitle txt
  | T.all isSpace txt = Left $ T.pack "Title cannot be empty or whitespace"
  | otherwise = Right $ Title txt

newtype MovieRuntime = MovieRuntime { value :: Int }
mkMovieRuntime :: Int -> Either Text MovieRuntime
mkMovieRuntime n
  | n < 0 = Left $ T.pack "Movie runtime cannot be negative"
  | otherwise = Right $ MovieRuntime n

data Movie = Movie
  { titleId :: TitleId
  , title :: Title
  , mRuntimeMinutes :: Maybe MovieRuntime
  , mReleaseYear :: Maybe Year
  }

type MoviesAPI
  =    AddMovie
  :<|> GetMovie
  :<|> GetAllMovies
  :<|> DeleteMovie

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

type AddMovie = ReqBody '[JSON] AddMovieReqBody :> Post '[JSON] MovieResp

type GetMovie = Capture "titleId" UUID :> Get '[JSON] MovieResp

type GetAllMovies = Get '[JSON] [MovieResp]

type DeleteMovie = Capture "titleId" UUID :> DeleteNoContent

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

addMovie :: (MonadPgConn m, MonadUUID m) => AddMovieReqBody -> m MovieResp
addMovie reqBody = do
  titleId <- TitleId <$> newUUID
  case validMovie titleId of
    Failure _ -> error "TODO"
    Success movie -> do
      conn <- askPgConn
      liftIO $ runBeamPostgres conn $ do
        let (dbTitle, dbMovie) = movieToDb movie
        runInsert $ insert showsGateDB.titles $ insertValues [dbTitle]
        runInsert $ insert showsGateDB.movies $ insertValues [dbMovie]
      return $ movieToResp movie
  where
    validMovie titleId = do
      title <- validationNel $ mkTitle reqBody.title
      runtimeMinutes <- case reqBody.runtimeMinutes of
        Nothing -> pure Nothing
        Just rm -> validationNel $ Just <$> mkMovieRuntime rm
      let year = fromIntegral <$> reqBody.releaseYear
      return $ Movie titleId title runtimeMinutes year

getMovie :: MonadPgConn m => UUID -> m MovieResp
getMovie
  = fmap (maybe (error "TODO") movieToResp)
  . getMovieFromDb
  . TitleId

class MonadIO m => MonadPgConn m where
  askPgConn :: m Connection

runPg :: MonadPgConn m => Pg a -> m a
runPg pg = do
  conn <- askPgConn
  liftIO $ runBeamPostgres conn pg

selectOne
  :: (MonadPgConn m, FromBackendRow Postgres (QExprToIdentity res), Projectible Postgres res)
  => Q Postgres db QBaseScope res -> m (Maybe (QExprToIdentity res))
selectOne query = runPg $ runSelectReturningOne $ select query

selectList
  :: (MonadPgConn m, FromBackendRow Postgres (QExprToIdentity res), Projectible Postgres res)
  => Q Postgres db QBaseScope res -> m [QExprToIdentity res]
selectList query = runPg $ runSelectReturningList $ select query

movieFromDB :: (DB.Title, DB.Movie) -> Movie
movieFromDB (title, movie) = Movie
  { titleId = TitleId title.titleId
  , title = Title title.title
  , mRuntimeMinutes = MovieRuntime . fromIntegral <$> movie.movieRuntimeMinutes
  , mReleaseYear = fromIntegral <$> movie.movieReleaseYear
  }

getMovieFromDb :: MonadPgConn m => TitleId -> m (Maybe Movie)
getMovieFromDb (TitleId titleId) = do
  results <- selectOne $ do
    title <- filter_ (\title -> title.titleId ==. val_ titleId
                            &&. title.titleType ==. val_ MovieType)
           $ all_ showsGateDB.titles
    movie <- oneToOne_ showsGateDB.movies movieTitleId title
    return (title, movie)
  return $ movieFromDB <$> results

getAllMovies :: MonadPgConn m => m [MovieResp]
getAllMovies = map movieToResp <$> do
  results <- selectList $ do
    movie <- all_ showsGateDB.movies
    title <- all_ showsGateDB.titles
    guard_ $ movie.movieTitleId `references_` title
    return (title, movie)
  return $ movieFromDB <$> results

deleteMovie :: (MonadPgConn m, MonadPgConn m) => UUID -> m NoContent
deleteMovie titleId = do
  runPg $ runDelete $ delete showsGateDB.titles (\title -> title.titleId ==. val_ titleId)
  return NoContent
