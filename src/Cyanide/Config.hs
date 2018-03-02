{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Config where

import Data.Ini.Config
import Data.Word
import qualified Data.Text as T
import System.IO
import System.IO.Error
import System.Exit
import Control.Exception
import System.Environment
import System.Directory

defaultConfig :: String
defaultConfig = "[DATABASE]\n"
             ++ "host = localhost\n"
             ++ "port = 5432\n"
             ++ "user = cyanide\n"
             ++ "password = cyanide\n"
             ++ "database = cyanide\n"
             ++ "\n"
             ++ "[EDITOR]\n"
             ++ "editor = \n"

getConfigDir :: IO String
getConfigDir = do
    homeDir <- getEnv "HOME"
    return $ homeDir ++ "/.config/cyanide"

getConfigLocation :: IO String
getConfigLocation = do
    dir <- getConfigDir
    return $ dir ++ "/cyanide.conf"

data Config = Config
    { databaseSection :: DatabaseConfig
    , editorSection :: EditorConfig
    }

data DatabaseConfig = DatabaseConfig
    { configHost     :: T.Text
    , configPort     :: Word16
    , configUser     :: T.Text
    , configPassword :: T.Text
    , configDatabase :: T.Text
    }

data EditorConfig = EditorConfig
    { editor :: T.Text
    }

configParser :: IniParser Config 
configParser = do
    dbase <- section "DATABASE" $ do
        host <- field "host"
        port <- fieldOf "port" number
        user <- field "user"
        password <- field "password"
        database <- field "database"
        return DatabaseConfig
                    { configHost     = host
                    , configPort     = port
                    , configUser     = user
                    , configPassword = password
                    , configDatabase = database
                    }
    edCfg <- section "EDITOR" $ do
        ed <- field "editor"
        return $ EditorConfig ed
    return $ Config dbase edCfg

getContentsSafe :: String -> IO (Either IOError String)
getContentsSafe path =
        catch
            (do s <- readFile path
                return $ Right s)
        handleError
    where handleError :: IOError -> IO (Either IOError String)
          handleError e = return $ Left e

getConfig :: IO (Either String Config)
getConfig = do
    configLocation <- getConfigLocation
    res <- getContentsSafe configLocation
    case res of
        Left err -> if isDoesNotExistError err
                        then configCreationPrompt
                        else return $ Left $ show err
        Right s -> return $ parseIniFile (T.pack s) configParser

configCreationPrompt :: IO (Either String Config)
configCreationPrompt = do
    configLocation <- getConfigLocation
    hPutStrLn stderr $ "the configuration file " ++ configLocation ++ " does not exist"
    saidYes <- getConfirmation "should I create it? [y/N] "
    if not saidYes
        then exitWith (ExitFailure 1)
        else do
            dir <- getConfigDir
            createDirectoryIfMissing True dir
            writeFile configLocation defaultConfig
            hPutStrLn stderr $ "I've created: " ++ configLocation
            hPutStrLn stderr "please edit it and then re-run cyanide"
            exitWith (ExitFailure 1)
  where getConfirmation :: String -> IO Bool
        getConfirmation prompt = do
            hPutStrLn stderr prompt
            line <- hGetLine stdin
            case line of
                "Y" -> return True
                "y" -> return True
                "N" -> return False
                "n" -> return False
                _ -> getConfirmation prompt
