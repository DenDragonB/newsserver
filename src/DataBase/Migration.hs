{-# LANGUAGE OverloadedStrings #-}

module DataBase.Migration where

import           System.Directory ( listDirectory )
import qualified System.IO as IO
import qualified Data.ByteString as BS
import           Database.PostgreSQL.Simple.Types
                    ( Query(Query), Only(fromOnly) )
import           Control.Monad.Reader (when)
import           Data.List (sort,isSuffixOf)

import           DataBase  

migrateDB :: DBPool -> IO ()
migrateDB pool = do
    Just v <- versionDB pool 
    files <- listDirectory "src/DataBase"
    let fs = filter (isSuffixOf ".sql") files
    mapM_ (\f -> when (f > v) $ migrateFile pool $ "src/DataBase/" <> f) $ sort fs

migrateFile :: DBPool -> FilePath -> IO ()
migrateFile pool file = do
    IO.withFile file IO.ReadMode $
        \ hdl -> do
            ups <- BS.hGetContents hdl
            _ <- execDB pool $ Query ups
            return ()

versionDB :: DBPool -> IO (Maybe String)
versionDB pool = do
    [isExist] <- queryDB pool $
            "SELECT EXISTS (SELECT 1 "
            <> "FROM information_schema.tables "
            <> "WHERE table_name = 'migrationhistory')" :: IO [Only Bool]
    case fromOnly isExist of
        False -> return $ Just "0000"
        _  -> do
            [v] <- queryDB pool $
                "SELECT MAX (FileNumber) FROM MigrationHistory" :: IO [Only String]
            return $ ( Just . fromOnly) v