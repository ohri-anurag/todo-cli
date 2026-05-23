{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Aeson qualified as Aeson
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
import Refined (refineError)
import Relude
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import Task qualified

data Command
  = AddTask Task.TaskWithoutSubTasks
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

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    AddTask task ->
      print task
    Setup method ->
      case method of
        Postgres -> do
          path <- getXdgDirectory XdgConfig "todo"
          createDirectoryIfMissing True path
          writeFileLBS (path </> "todo.config")
            $ Aeson.encode
            $ Postgres.Details
              { table = $$(NonEmptyText.make "table name"),
                schema = $$(NonEmptyText.make "schema name"),
                connString = $$(NonEmptyText.make "postgres connection string")
              }
