{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultilineStrings #-}

module Main (main) where

import Color qualified
import Control.Monad.Except (withExceptT)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (ZonedTime, getCurrentTimeZone, zonedTimeToUTC)
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Version (showVersion)
import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Session qualified
import NonEmptyText (NonEmptyText (..))
import NonEmptyText qualified as NonEmptyText
import Options.Applicative
  ( Parser,
    ParserInfo,
    ReadM,
    argument,
    auto,
    command,
    commandGroup,
    eitherReader,
    execParser,
    flag',
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
import Paths_todo_cli qualified as Paths
import Postgres.Details qualified as Postgres
import Postgres.Init qualified as Postgres.Init
import Postgres.Task qualified as Postgres
import Postgres.Task.Complete qualified as Postgres.Complete
import Postgres.Task.Insert qualified as Postgres.Insert
import Postgres.Task.Update qualified as Postgres.Update
import Relude hiding (id)
import Repeat qualified
import Setup qualified
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
  | Version

data SetupMethod = Postgres

nonEmptyTextReader :: ReadM NonEmptyText
nonEmptyTextReader = eitherReader (NonEmptyText.parse . toText)

zonedTimeReader :: ReadM ZonedTime
zonedTimeReader = str >>= ISO8601.iso8601ParseM

commandParser :: Parser Command
commandParser =
  flag' Version (mconcat [short 'v', long "version", help "Get the current version of todo"])
    <|> hsubparser
      ( mconcat
          [ commandGroup "Task CRUD",
            command "add"
              $ info (AddTask <$> addTaskOptionsParser)
              $ progDesc "Adds a new task",
            command "complete"
              $ info (CompleteTask <$> completeParser)
              $ progDesc "Mark a task (and all its sub-tasks) as finished",
            command "list"
              $ info (pure List)
              $ progDesc "List all the incomplete tasks",
            command "update"
              $ info (UpdateTask <$> updateParser <*> updateTaskOptionsParser)
              $ progDesc "Update an existing task"
          ]
      )
    <|> hsubparser
      ( mconcat
          [ commandGroup "Todo Setup",
            command "init"
              $ info (Init <$> setupMethodParser)
              $ progDesc "Initialise the task storage as configured via the setup command",
            command "setup"
              $ info (Setup <$> setupMethodParser)
              $ progDesc "Create a config file for the selected storage method. Currently only Postgres is supported."
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
  parent <- optional $ option auto $ mconcat [short 'p', long "parent", help "The ID of this task's parent"]
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
        parent = parent,
        remindAt = zonedTimeToUTC <$> remindAt,
        repeatAfter = repeatAfter,
        tags = nonEmpty $ mapMaybe (either (const Nothing) Just . NonEmptyText.parse) $ fold tags
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

displayError :: Error -> IO ()
displayError =
  TIO.putStrLn . Color.red . \case
    ConfigParseError err -> "Could not parse the config, error:\n" <> toText err
    PostgresConnectionError connectionError ->
      "Could not connect to postgres" <> maybe "" ((", error:\n" <>) . decodeUtf8) connectionError
    PostgresSesssionError err -> "Encountered a Postgres session error:\n" <> show err
    EmptyUpdateOperation -> "There must be something to update! Please provide some details that you want to update in the task."

withPostgresConnection :: (Hasql.Connection.Connection -> Postgres.Schema -> Postgres.TableName -> Setup.Palette -> ExceptT Error IO a) -> ExceptT Error IO a
withPostgresConnection f = do
  path <- lift $ getXdgDirectory XdgConfig "todo"
  Setup.Details {postgres = Postgres.Details {..}, palette} <-
    withExceptT ConfigParseError $ ExceptT $ Aeson.eitherDecodeFileStrict $ path </> "todo.config"
  let connSetting = Hasql.Connection.Setting.connection $ Hasql.Connection.Setting.Connection.string $ toText connString
  conn <- withExceptT PostgresConnectionError $ ExceptT $ Hasql.Connection.acquire [connSetting]
  a <- f conn schema table palette
  lift $ Hasql.Connection.release conn
  pure a

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    AddTask taskOptions -> do
      eitherError <- runExceptT
        $ withPostgresConnection
        $ \conn schema table _ ->
          withExceptT PostgresSesssionError
            $ ExceptT
            $ flip Hasql.Session.run conn
            $ Postgres.Insert.addTask schema table taskOptions

      bitraverse_ displayError (const $ TIO.putStrLn $ Color.green "Task added successfully!") eitherError
    CompleteTask index -> do
      eitherError <- runExceptT
        $ withPostgresConnection
        $ \conn schema table _ ->
          withExceptT PostgresSesssionError
            $ ExceptT
            $ flip Hasql.Session.run conn
            $ Postgres.Complete.completeTask schema table index
      bitraverse_ displayError (const $ TIO.putStrLn $ Color.green "Task completed successfully!") eitherError
    Init method ->
      case method of
        Postgres -> do
          eitherError <- runExceptT $ do
            withPostgresConnection $ \conn schema table _ ->
              withExceptT PostgresSesssionError
                $ ExceptT
                $ flip Hasql.Session.run conn
                $ Postgres.Init.initTaskTable schema table
          bitraverse_ displayError (const $ TIO.putStrLn $ Color.green "Initialsed configuration successfully!") eitherError
    List -> do
      tz <- getCurrentTimeZone
      eitherError <- runExceptT $ do
        withPostgresConnection $ \conn schema table palette -> do
          tasks <-
            fmap Postgres.unpackAll
              $ withExceptT PostgresSesssionError
              $ ExceptT
              $ flip Hasql.Session.run conn
              $ Postgres.listTasks schema table
          traverse_ (putStrLn . toString . Task.display tz palette) tasks
      bitraverse_ displayError (const $ pure ()) eitherError
    Setup method ->
      case method of
        Postgres -> do
          path <- getXdgDirectory XdgConfig "todo"
          createDirectoryIfMissing True path
          writeFileLBS (path </> "todo.config")
            $ Aeson.encode
            $ Setup.defaultDetails
    UpdateTask index Postgres.Insert.TaskOptions {..} -> do
      let updates =
            catMaybes
              [ Postgres.Update.UpdateDescription <$> description,
                Postgres.Update.UpdateDue <$> due,
                Postgres.Update.UpdateParent <$> parent,
                Postgres.Update.UpdateRemindAt <$> remindAt,
                Postgres.Update.UpdateRepeatAfter <$> repeatAfter,
                Postgres.Update.UpdateTags <$> tags
              ]
      eitherError <- runExceptT $ do
        neUpdates <- maybeToExceptT EmptyUpdateOperation . hoistMaybe $ nonEmpty updates
        withPostgresConnection $ \conn schema table _ ->
          withExceptT PostgresSesssionError
            $ ExceptT
            $ flip Hasql.Session.run conn
            $ Postgres.Update.updateTask schema table index neUpdates
      bitraverse_ displayError (const $ TIO.putStrLn $ Color.green "Task updated successfully!") eitherError
    Version ->
      putStrLn $ showVersion Paths.version
