module DataBase.Migration where

import Database

migrateDB :: DBPool -> IO ()
migrateDB pool = do
    Just v <- versionDB pool 
    files <- listDirectory "src/DataBase"
    mapM_ (\f -> when (f > v) $ migrateFile pool $ "src/DataBase/" <> f) $ sort files

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