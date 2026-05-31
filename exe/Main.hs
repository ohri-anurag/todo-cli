{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Except (withExceptT)
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (ZonedTime, getCurrentTimeZone, zonedTimeToUTC)
import Data.Time.Format.ISO8601 qualified as ISO8601
import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Session qualified
import Hasql.Transaction qualified
import Hasql.Transaction.Sessions qualified
import NonEmptyText (NonEmptyText (..))
import NonEmptyText qualified as NonEmptyText
import Options.Applicative
  ( Parser,
    ParserInfo,
    ReadM,
    argument,
    command,
    eitherReader,
    execParser,
    fullDesc,
    help,
    helpDoc,
    helper,
    hsubparser,
    info,
    long,
    maybeReader,
    metavar,
    option,
    progDesc,
    short,
    str,
    strOption,
  )
import Postgres.Details qualified as Postgres
import Postgres.Task qualified as Postgres
import Refined (refineError)
import Rel8 qualified
import Relude
import Repeat qualified
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import Task qualified

data Command
  = AddTask Task.TaskWithoutSubTasks
  | Init SetupMethod
  | List
  | Setup SetupMethod
  deriving (Show)

data SetupMethod = Postgres
  deriving (Show)

nonEmptyTextReader :: ReadM NonEmptyText
nonEmptyTextReader = eitherReader (bimap show NonEmptyText . refineError . toText)

zonedTimeReader :: ReadM ZonedTime
zonedTimeReader = str >>= ISO8601.iso8601ParseM

commandParser :: Parser Command
commandParser =
  hsubparser
    ( mconcat
        [ command "add"
            $ info (AddTask <$> taskParser)
            $ progDesc "Adds a new task",
          command "init"
            $ info (Init <$> setupMethodParser)
            $ progDesc "Initialise the task storage as configured via the setup command",
          command "list"
            $ info (pure List)
            $ progDesc "List all the incomplete tasks",
          command "setup"
            $ info (Setup <$> setupMethodParser)
            $ progDesc "Create a config file for the selected storage method. Currently only Postgres is supported."
        ]
    )
    <**> helper

taskParser :: Parser Task.TaskWithoutSubTasks
taskParser = do
  description <-
    argument nonEmptyTextReader
      $ mconcat [metavar "DESC", help "A text based description of the task"]
  due <- option zonedTimeReader $ mconcat [short 'd', long "due", help "Due date in ISO8601 format (yyyy-MM-ddThh:mm:ss+hh:mm)."]
  remindAt <- option zonedTimeReader $ mconcat [long "remind-at", help "When to receive a reminder for this task in ISO8601 format (yyyy-MM-ddThh:mm:ss+hh:mm)."]
  repeatAfter <-
    option (maybeReader Repeat.parse)
      $ mconcat
        [ short 'r',
          long "repeat",
          helpDoc
            $ Just
              """
              Possible values are:
              - daily
              - weekly
              - monthly
              - yearly
              - n (days|weeks|months|years), n >= 2
              - mon,tue,wed,thu,fri,sat,sun. Each of the days is optional, provided that at least two days are present.
              Example: mon,tue,fri
              """
        ]
  tags <- fmap (Text.splitOn ",") $ strOption $ mconcat [short 't', long "tags", help "Comma separated list of tags"]
  pure
    Task.Task
      { description = description,
        due = Just $ zonedTimeToUTC due,
        remindAt = Just $ zonedTimeToUTC remindAt,
        repeatAfter = Just repeatAfter,
        subTasks = Proxy,
        tags = nonEmpty $ mapMaybe NonEmptyText.parse tags
      }

setupMethodParser :: Parser SetupMethod
setupMethodParser =
  hsubparser
    ( mconcat
        [ command "postgres"
            $ info (pure Postgres)
            $ progDesc "Use Postgres as the storage"
        ]
    )

parserInfo :: ParserInfo Command
parserInfo = info commandParser fullDesc

data Error
  = ConfigParseError String
  | PostgresConnectionError Hasql.Connection.ConnectionError
  | PostgresSesssionError Hasql.Session.SessionError
  deriving (Show)

withPostgresConnection :: (Hasql.Connection.Connection -> Postgres.Schema -> Postgres.TableName -> ExceptT Error IO a) -> ExceptT Error IO a
withPostgresConnection f = do
  path <- lift $ getXdgDirectory XdgConfig "todo"
  Postgres.Details {..} <- withExceptT ConfigParseError $ ExceptT $ Aeson.eitherDecodeFileStrict $ path </> "todo.config"
  let connSetting = Hasql.Connection.Setting.connection $ Hasql.Connection.Setting.Connection.string $ toText connString
  conn <- withExceptT PostgresConnectionError $ ExceptT $ Hasql.Connection.acquire [connSetting]
  a <- f conn schema table
  lift $ Hasql.Connection.release conn
  pure a

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    AddTask task -> do
      tz <- getCurrentTimeZone
      eitherError <- runExceptT $ do
        withPostgresConnection $ \conn schema table -> do
          tasks <- fmap Postgres.unpackAll
            $ withExceptT PostgresSesssionError
            $ ExceptT
            $ flip Hasql.Session.run conn
            . Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write
            $ do
              Hasql.Transaction.statement ()
                . Rel8.run_
                . Rel8.insert
                $ Postgres.insertTask schema table task
              Hasql.Transaction.statement ()
                . Rel8.run
                . Rel8.select
                $ Postgres.listNonCompletedTasks schema table
          lift $ traverse_ (putStrLn . toString . Task.display tz) tasks

      whenLeft_ eitherError print
    Init method ->
      case method of
        Postgres -> do
          void . runExceptT $ do
            withPostgresConnection $ \conn (Postgres.Schema schema) (Postgres.TableName table) ->
              withExceptT PostgresSesssionError
                $ ExceptT
                $ flip Hasql.Session.run conn
                $ Hasql.Session.sql
                  [i|
                  create table if not exists "#{toText schema}"."#{toText table}" (
                  	"created_at" timestamptz not null,
                  	"updated_at" timestamptz not null,
                  	"id" bigint generated always as identity primary key,
                  	"is_completed" bool not null,
                  	"description" text not null,
                   	"due" timestamptz,
                  	"remind_at" timestamptz,
                  	"repeat_after" text,
                  	"parent" bigint,
                  	"tags" text);
                |]
    List -> do
      tz <- getCurrentTimeZone
      eitherError <- runExceptT $ do
        withPostgresConnection $ \conn schema table -> do
          tasks <-
            fmap Postgres.unpackAll
              $ withExceptT PostgresSesssionError
              $ ExceptT
              $ flip Hasql.Session.run conn
              . Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write
              $ Hasql.Transaction.statement ()
              . Rel8.run
              . Rel8.select
              $ Postgres.listNonCompletedTasks schema table
          traverse_ (putStrLn . toString . Task.display tz) tasks
      whenLeft_ eitherError print
    Setup method ->
      case method of
        Postgres -> do
          path <- getXdgDirectory XdgConfig "todo"
          createDirectoryIfMissing True path
          writeFileLBS (path </> "todo.config")
            $ Aeson.encode
            $ Postgres.Details
              { table = Postgres.TableName $$(NonEmptyText.make "table name"),
                schema = Postgres.Schema $$(NonEmptyText.make "schema name"),
                connString = $$(NonEmptyText.make "postgres connection string")
              }
