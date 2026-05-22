{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Data.Aeson qualified as Aeson
import NonEmptyText (NonEmptyText (..))
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
import Refined (refineError, unrefine)
import Relude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Task qualified

data Command
  = AddTask Task.TaskWithoutSubTasks
  | Setup SetupDetails
  deriving (Show)

data SetupDetails = SetupDetails
  { configPath :: NonEmptyText,
    method :: SetupMethod
  }
  deriving (Show)

data SetupMethod = Postgres Postgres.Details
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
          command "setup"
            $ info (Setup <$> setupDetailsParser)
            $ progDesc "Setup the storage for todo"
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

setupDetailsParser :: Parser SetupDetails
setupDetailsParser = do
  method <- setupMethodParser
  configPath <-
    argument nonEmptyTextReader
      $ mconcat [metavar "CONFIG_DIR", help "Path to a directory where todo will create and store the its config"]
  pure SetupDetails {..}

setupMethodParser :: Parser SetupMethod
setupMethodParser = Postgres <$> postgresDetailsParser

postgresDetailsParser :: Parser Postgres.Details
postgresDetailsParser = do
  table <-
    argument nonEmptyTextReader
      $ mconcat [metavar "TABLE", help "Postgres table name for storing tasks"]
  schema <-
    argument nonEmptyTextReader
      $ mconcat [metavar "SCHEMA", help "Schema in which the table should exist"]
  connString <-
    argument nonEmptyTextReader
      $ mconcat [metavar "CONN_STR", help "The connection string for your Postgres database"]
  pure Postgres.Details {..}

parserInfo :: ParserInfo Command
parserInfo = info commandParser fullDesc

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    AddTask task ->
      print task
    Setup SetupDetails {configPath = NonEmptyText configPath, method} ->
      case method of
        Postgres details -> do
          let path = toString . unrefine $ configPath
          createDirectoryIfMissing True path
          writeFileLBS (path </> "todo.config") $ Aeson.encode details
