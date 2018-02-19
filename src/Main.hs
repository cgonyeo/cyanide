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
        Right (Config.Config h po u pa d) -> do
            conn <- Postgres.newDBConn h po u pa d
            Postgres.initDb conn
            App.run conn
