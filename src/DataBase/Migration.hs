{-# LANGUAGE OverloadedStrings #-}

module DataBase.Migration where

import           Control.Monad.Reader             (when)
import qualified Data.ByteString                  as BS
import           Data.List                        (isSuffixOf, sort)
import           Database.PostgreSQL.Simple.Types (Only (fromOnly),
                                                   Query (Query))
import           System.Directory                 (listDirectory)
import qualified System.IO                        as IO

import           DataBase

migrateDB :: DBPool -> IO ()
migrateDB pool = do
    Just v <- versionDB pool
    let sqlDir = "src/DataBase/sql/"
    files <- listDirectory sqlDir
    let fs = filter (isSuffixOf ".sql") files
    mapM_ (\f -> when (f > v <> ".sql") $ migrateFile pool $ sqlDir <> f) $ sort fs

migrateFile :: DBPool -> FilePath -> IO ()
migrateFile pool file = do
    IO.withFile file IO.ReadMode $
        \ hdl -> do
            putStrLn file
            ups <- BS.hGetContents hdl
            _ <- execDB pool $ Query ups
            return ()

versionDB :: DBPool -> IO (Maybe String)
versionDB pool = do
    [isExist] <- queryDB pool $
            "SELECT EXISTS (SELECT 1 "
            <> "FROM information_schema.tables "
            <> "WHERE table_name = 'migrationhistory')" :: IO [Only Bool]
    if fromOnly isExist then (do
        [v] <- queryDB pool
            "SELECT MAX (FileNumber) FROM MigrationHistory" :: IO [Only String]
        putStrLn $ fromOnly v
        return $ ( Just . fromOnly) v) else return $ Just "000"
