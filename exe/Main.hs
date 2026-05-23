{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Except (withExceptT)
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
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
    helper,
    hsubparser,
    info,
    metavar,
    progDesc,
  )
import Postgres.Details qualified as Postgres
import Postgres.Task qualified as Postgres
import Refined (refineError)
import Rel8 qualified
import Relude
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import Task qualified

data Command
  = AddTask Task.TaskWithoutSubTasks
  | Init SetupMethod
  | Setup SetupMethod
  deriving (Show)

data SetupMethod = Postgres
  deriving (Show)

nonEmptyTextReader :: ReadM NonEmptyText
nonEmptyTextReader = eitherReader (bimap show NonEmptyText . refineError . toText)

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
  pure
    Task.Task
      { description = description,
        due = Nothing,
        remindAt = Nothing,
        repeatAfter = Nothing,
        subTasks = Proxy,
        tags = Nothing
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

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    AddTask task -> do
      eitherError <- runExceptT $ do
        path <- lift $ getXdgDirectory XdgConfig "todo"
        Postgres.Details {..} <- withExceptT ConfigParseError $ ExceptT $ Aeson.eitherDecodeFileStrict $ path </> "todo.config"
        let connSetting = Hasql.Connection.Setting.connection $ Hasql.Connection.Setting.Connection.string $ toText connString
        conn <- withExceptT PostgresConnectionError $ ExceptT $ Hasql.Connection.acquire [connSetting]
        tasks <- withExceptT PostgresSesssionError
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
        lift $ do
          print tasks
          Hasql.Connection.release conn

      whenLeft_ eitherError print
    Init method ->
      case method of
        Postgres -> do
          void . runExceptT $ do
            path <- lift $ getXdgDirectory XdgConfig "todo"
            Postgres.Details {..} <- withExceptT ConfigParseError $ ExceptT $ Aeson.eitherDecodeFileStrict $ path </> "todo.config"
            let connSetting = Hasql.Connection.Setting.connection $ Hasql.Connection.Setting.Connection.string $ toText connString
            conn <- withExceptT PostgresConnectionError $ ExceptT $ Hasql.Connection.acquire [connSetting]
            withExceptT PostgresSesssionError
              $ ExceptT
              $ flip Hasql.Session.run conn
              $ Hasql.Session.sql
                [i|
                  create table if not exists "public"."tasks" (
                  	"created_at" timestamptz not null,
                  	"updated_at" timestamptz not null,
                  	"id" bigint generated always as identity primary key,
                  	"is_sub_task" bool not null,
                  	"is_completed" bool not null,
                  	"description" text not null,
                   	"due" timestamptz,
                  	"remind_at" timestamptz,
                  	"repeat_after" bigint,
                  	"sub_tasks" text,
                  	"tags" text);
                |]

            lift $ Hasql.Connection.release conn
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
