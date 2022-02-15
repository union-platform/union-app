-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents command line interface for migrations tool.
module Command.Migrations
  ( MigrationsCommand(..)
  , migrationsParser
  , migrations
  ) where

import Relude

import qualified Data.Text as T

import Control.Monad.Except (catchError)
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)
import Options.Applicative
  (Parser, command, help, hsubparser, info, metavar, progDesc, strArgument)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Core.Db (getAppliedMigrations, pruneDb, runTransaction, showMigration)
import Core.Error (showErr, toAppError)
import Core.Has (grab)
import Core.Logging (logError)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import Union.Application (ErrorType(DbError), runUnion)
import Union.Configuration (UnionConfig(..), dcMigrations)


-- | Available migrations commands.
data MigrationsCommand = Create String | List | Prune
  deriving stock (Show, Eq)

-- | Parser for all available migrations commands.
migrationsParser :: Parser MigrationsCommand
migrationsParser = hsubparser $ mconcat
  [ command "create" . info (Create <$> argName) $ progDesc
    "Create new migration"
  , command "list" . info (pure List) $ progDesc "List applied migrations"
  , command "prune" . info (pure Prune) $ progDesc
    "Prune DB, drop all data and schema"
  ]

-- | Parser for name argument.
argName :: Parser String
argName = strArgument $ mconcat
  [metavar "<name>", help "Migration name (really short description)"]

-- | Migrations command interpreter.
migrations :: Maybe FilePath -> MigrationsCommand -> IO ()
migrations configPath cmd = runUnion configPath $ case cmd of
  (Create name) -> do
    UnionConfig { ucDatabase } <- grab @UnionConfig
    now                        <- liftIO getCurrentTime
    let
      (year, _, _)         = toGregorian $ utctDay now
      timestamp :: Integer = round $ utcTimeToPOSIXSeconds now * 1000
      file =
        dcMigrations ucDatabase </> show timestamp <> "-" <> name <> ".sql"
    liftIO (doesFileExist file) >>= \case
      False -> do
        writeFile file $ licenseHeader year
        putStrLn $ "Migration " <> file <> " was successfully created"
      True -> putStrLn $ "Migration " <> file <> " already exists, skipping"
  List -> do
    mList <- toAppError DbError getAppliedMigrations
      `catchError` \err -> logError (showErr err) >> fail "Check logs 🠑"
    putTextLn "Following migrations applied to Union:"
    mapM_ (putTextLn . showMigration) mList
  Prune -> do
    toAppError DbError (runTransaction pruneDb)
      `catchError` \err -> logError (showErr err) >> fail "Check logs 🠑"
    putTextLn "Now you schema is clean"

-- | Returns license header for new migration files.
-- Note, that here we replace '%' with ':' - we cannot use ':' directly because
-- of bug in reuse lib, it parses this function as real license id for file.
licenseHeader :: Integer -> String
licenseHeader year = toString . T.replace "%" ":" $ T.intercalate
  "\n"
  [ "-- SPDX-FileCopyrightText% " <> show year <> " Union"
  , "--"
  , "-- SPDX-License-Identifier% AGPL-3.0-or-later"
  ]
