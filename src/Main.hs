module Main where

import qualified Cyanide.Config as Config
import qualified Cyanide.Data.Postgres as Postgres

import qualified Cyanide.UI.App as App

import System.Exit
import System.IO

main :: IO ()
main = do
    config <- Config.getConfig
    case config of
        Left err -> do
            hPutStrLn stderr err
            exitWith (ExitFailure 1)
        Right c@(Config.Config (Config.DatabaseConfig h po u pa d) _) -> do
            conn <- Postgres.newDBConn h po u pa d
            Postgres.initDb conn
            App.run conn c
