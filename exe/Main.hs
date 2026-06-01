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
    auto,
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
import Postgres.Task.Insert qualified as Postgres.Insert
import Postgres.Task.Update qualified as Postgres.Update
import Refined (refineError)
import Rel8 qualified
import Relude
import Repeat qualified
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import Task qualified

data Command
  = AddTask Postgres.Insert.AddTaskOptions
  | CompleteTask Int64
  | Init SetupMethod
  | List
  | Setup SetupMethod
  | UpdateTask Int64 Postgres.Insert.UpdateTaskOptions

data SetupMethod = Postgres

nonEmptyTextReader :: ReadM NonEmptyText
nonEmptyTextReader = eitherReader (bimap show NonEmptyText . refineError . toText)

zonedTimeReader :: ReadM ZonedTime
zonedTimeReader = str >>= ISO8601.iso8601ParseM

commandParser :: Parser Command
commandParser =
  hsubparser
    ( mconcat
        [ command "add"
            $ info (AddTask <$> addTaskOptionsParser)
            $ progDesc "Adds a new task",
          command "complete"
            $ info (CompleteTask <$> completeParser)
            $ progDesc "Mark a task as finished",
          command "init"
            $ info (Init <$> setupMethodParser)
            $ progDesc "Initialise the task storage as configured via the setup command",
          command "list"
            $ info (pure List)
            $ progDesc "List all the incomplete tasks",
          command "setup"
            $ info (Setup <$> setupMethodParser)
            $ progDesc "Create a config file for the selected storage method. Currently only Postgres is supported.",
          command "update"
            $ info (UpdateTask <$> updateParser <*> updateTaskOptionsParser)
            $ progDesc "Update an existing task"
        ]
    )
    <**> helper

taskOptionsParser :: (forall a. Parser a -> Parser (f a)) -> Parser (Postgres.Insert.TaskOptions f)
taskOptionsParser toF = do
  description <-
    toF
      $ argument nonEmptyTextReader
      $ mconcat [metavar "DESC", help "A text based description of the task"]
  due <- optional $ option zonedTimeReader $ mconcat [short 'd', long "due", help "Due date in ISO8601 format (yyyy-MM-ddThh:mm:ss+hh:mm)."]
  remindAt <- optional $ option zonedTimeReader $ mconcat [long "remind-at", help "When to receive a reminder for this task in ISO8601 format (yyyy-MM-ddThh:mm:ss+hh:mm)."]
  repeatAfter <-
    optional
      $ option (maybeReader Repeat.parse)
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
  tags <- optional $ fmap (Text.splitOn ",") $ strOption $ mconcat [short 't', long "tags", help "Comma separated list of tags"]
  pure
    Postgres.Insert.TaskOptions
      { description = description,
        due = zonedTimeToUTC <$> due,
        remindAt = zonedTimeToUTC <$> remindAt,
        repeatAfter = repeatAfter,
        tags = nonEmpty $ mapMaybe NonEmptyText.parse $ fold tags
      }

addTaskOptionsParser :: Parser Postgres.Insert.AddTaskOptions
addTaskOptionsParser = taskOptionsParser (fmap Identity)

updateTaskOptionsParser :: Parser Postgres.Insert.UpdateTaskOptions
updateTaskOptionsParser = taskOptionsParser optional

idParser :: String -> Parser Int64
idParser message =
  argument auto $ mconcat [metavar "INDEX", help message]

updateParser :: Parser Int64
updateParser = idParser "The index of the task that needs to be updated"

completeParser :: Parser Int64
completeParser = idParser "The index of the task that needs to be marked as completed"

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
  | EmptyUpdateOperation
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
    AddTask taskOptions -> do
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
                $ Postgres.Insert.insertTask schema table taskOptions
              Hasql.Transaction.statement ()
                . Rel8.run
                . Rel8.select
                $ Postgres.listNonCompletedTasks schema table
          lift $ traverse_ (putStrLn . toString . Task.display tz) tasks

      whenLeft_ eitherError print
    CompleteTask index -> do
      eitherError <- runExceptT
        $ withPostgresConnection
        $ \conn schema table -> do
          withExceptT PostgresSesssionError
            $ ExceptT
            $ flip Hasql.Session.run conn
            . Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write
            $ Hasql.Transaction.statement ()
            . Rel8.run_
            . Rel8.update
            $ Postgres.completeTask schema table index
      whenLeft_ eitherError print
    Init method ->
      case method of
        Postgres -> do
          eitherError <- runExceptT $ do
            withPostgresConnection $ \conn (Postgres.Schema schema) (Postgres.TableName table) ->
              withExceptT PostgresSesssionError
                $ ExceptT
                $ flip Hasql.Session.run conn
                $ Hasql.Session.sql
                  [i|
                  create table if not exists "#{toText schema}"."#{toText table}" (
                  	"created_at" timestamptz not null,
                  	"updated_at" timestamptz not null,
                  	"id" bigint generated by default as identity primary key,
                  	"is_completed" bool not null,
                  	"description" text not null,
                   	"due" timestamptz,
                  	"remind_at" timestamptz,
                  	"repeat_after" text,
                  	"parent" bigint,
                  	"tags" text);
                |]
          whenLeft_ eitherError print
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
    UpdateTask index Postgres.Insert.TaskOptions {..} -> do
      let updates =
            catMaybes
              [ Postgres.Update.UpdateDescription <$> description,
                Postgres.Update.UpdateDue <$> due,
                Postgres.Update.UpdateRemindAt <$> remindAt,
                Postgres.Update.UpdateRepeatAfter <$> repeatAfter,
                Postgres.Update.UpdateTags <$> tags
              ]
      eitherError <- runExceptT $ do
        neUpdates <- maybeToExceptT EmptyUpdateOperation . hoistMaybe $ nonEmpty updates
        withPostgresConnection $ \conn schema table ->
          withExceptT PostgresSesssionError
            $ ExceptT
            $ flip Hasql.Session.run conn
            . Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write
            $ Hasql.Transaction.statement ()
            . Rel8.run_
            . Rel8.update
            $ Postgres.Update.updateTask schema table index neUpdates
      whenLeft_ eitherError print
