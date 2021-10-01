{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Postgres where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.UTF8                 as BS
import           Data.Maybe                           (listToMaybe)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           Database.PostgreSQL.Simple.Types
import           Exceptions

import           DataBase
import DataBase.Types
import           Logger

import Data.Time ( Day )

-- Names of tables in database
data Tables = Authors | Categories | Drafts | News | Tags | Users
    deriving Show

-- Names of columns in all tables of database
data Fields = Id | UserId | About | CatName | Tag
    deriving Show

type ItemId = Int

existItemByField ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    , ToField a
    ) => Tables -> Fields -> a -> m Bool
existItemByField table field value = do
    isExist <- queryWithExcept
                (Query $ "SELECT EXISTS (SELECT id FROM "
                    <> BS.fromString (show table) <> " WHERE "
                    <> BS.fromString (show field) <> " = ?);")
                [value]
    return (maybe False fromOnly $ listToMaybe isExist)

selectMaybeItemByField ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    , ToField a, FromRow b
    ) =>  Tables -> Fields -> a -> m (Maybe b)
selectMaybeItemByField table field value = do
    items <- queryWithExcept
                (Query $ "SELECT * FROM "
                    <> BS.fromString (show table) <> " WHERE "
                    <> BS.fromString (show field) <> " = ?);")
                [value]
    return $ listToMaybe items

insertAuthorWithPrameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => ItemId -> String -> m (Maybe Author)
insertAuthorWithPrameters userId about = do
    items <- queryWithExcept
        "INSERT INTO Authors (UserId, About) VALUES (?,?) RETURNING *;"
        (userId,about)
    return $ listToMaybe items

updateAuthorWithParameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => ItemId -> Maybe ItemId -> Maybe String -> m (Maybe Author)
updateAuthorWithParameters authorId userId about = do
    items <- queryWithExcept
        (Query "UPDATE Authors SET "
            <> "UserId = COALESCE (?,userid),"
            <> "About = COALESCE (?,about)"
            <> "WHERE Id = ? RETURNING *;")
        (userId,about,authorId)
    return $ listToMaybe items

getAuthorWithParameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> Maybe ItemId -> [( String , Maybe String )] -> m [Author]
getAuthorWithParameters authorId userId param = do
    queryWithExcept
        (Query $ "SELECT id,userid,about FROM authors WHERE"
            <> "(id = COALESCE (?, id))"
            <> "AND (userid = COALESCE (?, userid))"
            <> "ORDER BY id "
            <> getLimitOffsetBS param <> ";")
        (authorId, userId)

deleteItemFromTableById ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Tables -> ItemId -> m ()
deleteItemFromTableById table id = do
    void $ execWithExcept
        (Query $ "DELETE FROM " <> BS.fromString (show table) <> " WHERE id = ?;")
        [id]

insertCategoryWithPrameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => String -> ItemId -> m (Maybe Category)
insertCategoryWithPrameters name parentId = do
    items <- queryWithExcept
        "INSERT INTO Categories (CatName, Parent) VALUES (?,?) RETURNING * ;"
        (name,parentId)
    return $ listToMaybe items

updateCategoryWithParameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => ItemId -> Maybe ItemId -> Maybe String -> m (Maybe Category)
updateCategoryWithParameters categoryId parentId name = do
    items <- queryWithExcept
        (Query "UPDATE Categories SET "
            <> "CatName = COALESCE (?, CatName),"
            <> "Parent = COALESCE (?, Parent )"
            <> "WHERE Id = ? RETURNING *;")
        (name,parentId,categoryId)
    return $ listToMaybe items

getCategoryWithParameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> Maybe ItemId -> Maybe String 
    -> [( String , Maybe String )] -> m [Category]
getCategoryWithParameters categoryId parentId name param = do
    queryWithExcept
        (Query $ "SELECT id,CatName,Parent FROM Categories WHERE"
            <> "(id = COALESCE (?, id))"
            <> "AND (CatName = COALESCE (?, CatName)) "
            <> "AND (Parent = COALESCE (?, Parent))"
            <> "ORDER BY Parent,CatName,Id "
            <> getLimitOffsetBS param <> ";")
        (categoryId, name, parentId)

selectAuthorIdByToken ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => String -> m (Maybe ItemId)
selectAuthorIdByToken token = do
    items <- queryWithExcept
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ? );")
        [token]
    return $ fromOnly <$> listToMaybe items

selectTagsByIds ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => [Int] -> m [ItemId]
selectTagsByIds ids = do
    items <- queryWithExcept
        (Query "SELECT id FROM Tags WHERE"
            <> "(array_position (?,id) IS NOT NULL)")
        [PGArray ids]
    return $ fromOnly <$> items

insertDraftWithPrameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => String -> ItemId -> ItemId -> String -> String -> [String]
    -> m (Maybe ItemId)
insertDraftWithPrameters header authorId categoryId content mainPhoto photos = do
    items <- queryWithExcept
        (Query "INSERT INTO Drafts "
            <> "(Header,RegDate,News,Author,Category,Content,MainPhoto,Photos)"
            <> " VALUES ( ?,NOW(),0,?,?,?,?, ?) RETURNING Id;")
        (header, authorId,categoryId,content,mainPhoto, PGArray photos)
    return $ fromOnly <$> listToMaybe items

updateDraftToTags ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> [ItemId] -> m ()
updateDraftToTags draftId tags = do
    if null tags
        then do
            void $ execWithExcept
                "Delete from draftstotags where draftid=?;"
                [draftId]
        else do
            void $ execWithExcept
                (Query "Delete from draftstotags where draftid=?;"
                    <> "Insert into draftstotags (draftid,tagid) "
                    <> "(SELECT ?,UNNEST (?)) ON CONFLICT DO NOTHING;")
                ( draftId , draftId , PGArray tags)

selectDraftById ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> m (Maybe Draft)
selectDraftById draftId = do
    items <- queryWithExcept
        (Query "SELECT d.Id,d.News,d.Header,d.RegDate,d.Author,d.Category,"
            <> "array_remove(array_agg(t.tagid),NULL),d.Content,d.MainPhoto,d.Photos "
            <> "FROM Drafts d "
            <> "LEFT OUTER JOIN draftstotags t ON t.draftid=d.id "
            <> "WHERE d.Id = ? "
            <> "GROUP BY d.Id,d.News,d.Header,d.RegDate,d.Author,d.Category,d.Content,d.MainPhoto,d.Photos")
        [draftId]
    return $ listToMaybe items

existDraftByIdAndAuthor ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => ItemId -> Maybe ItemId -> m Bool
existDraftByIdAndAuthor draftId authorId = do
    isExist <- queryWithExcept
        (Query "SELECT EXISTS (SELECT id FROM Drafts WHERE Id = ? AND Author = ?);")
        (draftId,authorId)
    return (maybe False fromOnly $ listToMaybe isExist)

updateDraftWithPrameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => ItemId -> Maybe String -> Maybe ItemId 
    -> Maybe String -> Maybe String -> Maybe [String]
    -> m ()
updateDraftWithPrameters draftId header categoryId content mainPhoto photos = do
    void $ execWithExcept
        (Query "UPDATE Drafts SET "
            <> "Header = COALESCE (?, Header ),"
            <> "Category = COALESCE (?, Category ),"
            <> "Content = COALESCE (?, Content ),"
            <> "MainPhoto = COALESCE (?, MainPhoto ),"
            <> "Photos = COALESCE (?, Photos )"
            <> "WHERE Id = ? ;")
        ( header
        , categoryId
        , content
        , mainPhoto
        , PGArray <$> photos
        , draftId)

insertNewsByDraftId::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => ItemId -> m (Maybe ItemId)
insertNewsByDraftId draftId = do
    items <- queryWithExcept
        (Query "INSERT INTO News "
            <> "(Header,RegDate,Author,Category,Content,MainPhoto,Photos)"
            <> "(SELECT Header,RegDate,Author,Category,Content,MainPhoto,Photos "
            <> "FROM Drafts WHERE Id = ? ) RETURNING Id;")
        [draftId]
    return $ fromOnly <$> listToMaybe items

insertNewsToTagsUpdateDraft ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> ItemId -> m ()
insertNewsToTagsUpdateDraft newsId draftId = do
    void $ execWithExcept
        (Query "INSERT INTO newstotags (newsid,tagid) "
            <> "SELECT ?,tagid FROM draftstotags WHERE draftid=?;"
            <> "UPDATE Drafts SET News = ? WHERE Id = ? ;")
        (newsId,draftId,newsId,draftId)

updateNewsFromDraft ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> ItemId -> m ()
updateNewsFromDraft newsId draftId = do
    void $ execWithExcept
        (Query "UPDATE News SET "
            <> "(Header,RegDate,Author,Category,Content,MainPhoto,Photos) = "
            <> "(SELECT Header,RegDate,Author,Category,Content,MainPhoto,Photos "
            <> "FROM Drafts WHERE Id = ?) WHERE Id = ? ;"
            <> "DELETE FROM newstotags WHERE newsid=?;"
            <> "INSERT INTO newstotags (newsid,tagid) "
            <> "SELECT ?,tagid FROM draftstotags WHERE draftid=?;" )
        (draftId,newsId,newsId,newsId,draftId)

selectNewsWithParameters::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> Maybe Day -> Maybe Day -> Maybe Day -> Maybe String 
    -> Maybe Int -> Maybe [Int] -> Maybe [Int] -> Maybe String -> Maybe String
    -> Maybe String -> BS.ByteString -> BS.ByteString
    -> m [Posts]
selectNewsWithParameters newsId newsDay newsDayLT newsDayGT newsAuthor
    newsTag newsTagALL newsTagIn newsHeader newsContent newsSearch 
    sortBy limitOffset = do
    queryWithExcept
        (Query $ "WITH filters AS (SELECT"
            <> " ?::INT AS news_id,"
            <> " ?::DATE AS ndate,"
            <> " ?::DATE AS ndate_LT,"
            <> " ?::DATE AS ndate_GT,"
            <> " ? AS nauthor,"
            <> " ?::INT AS ntag,"
            <> " ?::INT[] AS ntag_ALL,"
            <> " ?::INT[] AS ntag_IN,"
            <> " ? AS nheader,"
            <> " ? AS ncontent,"
            <> " lower(?) AS substring)"
            <> "SELECT n.Id, n.Header, n.RegDate, u.UserName, c.CatName, array_agg(t.tag) as tags, "
            <> "n.Content, n.MainPhoto, n.Photos "
            <> "FROM filters, News n"
            -- Add Name of Author
            <> " LEFT OUTER JOIN (Authors a JOIN Users u on a.userid = u.id)"
            <> " ON n.Author = a.id "
            -- Add name of Category
            <> " LEFT OUTER JOIN Categories c ON n.Category = c.id "
            -- Add names of Tags
            <> "LEFT OUTER JOIN newstotags nt ON nt.newsid=n.id "
            <> "LEFT JOIN Tags t ON t.id = nt.tagid "
            <> "WHERE "
            -- Add selection
            <> "(filters.news_id IS NULL OR n.id = filters.news_id) "
            <> "AND (filters.ndate IS NULL OR n.RegDate = filters.ndate) "
            <> "AND (filters.ndate_LT IS NULL OR n.RegDate < filters.ndate_LT) "
            <> "AND (filters.ndate_GT IS NULL OR n.RegDate > filters.ndate_GT) "
            <> "AND (filters.nauthor IS NULL OR strpos(u.UserName,filters.nauthor)>0) "
            <> "AND (filters.ntag IS NULL OR filters.ntag IN (SELECT tagid FROM newstotags WHERE newsid =n.Id)) "
            <> "AND (filters.ntag_ALL IS NULL OR "
            <> "     filters.ntag_ALL <@ (SELECT array_agg(tagid) FROM newstotags WHERE newsid=n.Id)) "
            <> "AND (filters.ntag_IN IS NULL OR "
            <> "     filters.ntag_IN && (SELECT array_agg(tagid) FROM newstotags WHERE newsid=n.Id)) "
            <> "AND (filters.nheader IS NULL OR strpos(n.Header,filters.nheader) > 0) "
            <> "AND (filters.ncontent IS NULL OR strpos(n.Content,filters.ncontent) > 0) "
            -- Add search
            <> "AND (filters.substring IS NULL OR "
            <> "     strpos (lower(n.Content),filters.substring) > 0 OR "
            <> "     strpos (lower(n.Header),filters.substring) > 0 OR "
            <> "     strpos (lower(u.UserName),filters.substring) > 0 OR "
            <> "     strpos (lower(t.tag),filters.substring) > 0 OR "
            <> "     strpos (lower(c.CatName),filters.substring) > 0) "
            -- group elements to correct work array_agg(tag)
            <> "GROUP BY n.Id, n.Header, n.RegDate, u.UserName, c.CatName, "
            <> "n.Content, n.MainPhoto, n.Photos "
            -- Add sorting
            <> sortBy
            -- Add pagination
            <> limitOffset
            <> ";")
        ( newsId, newsDay, newsDayLT, newsDayGT, newsAuthor
        , newsTag, PGArray <$> newsTagALL, PGArray <$> newsTagIn
        , newsHeader, newsContent, newsSearch)

insertTagWitnName ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => String -> m (Maybe TagType)
insertTagWitnName name = do
    items <- queryWithExcept
        "INSERT INTO Tags (Tag) VALUES (?) RETURNING * ;"
        [name]
    return $ listToMaybe items     

updateTagById ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => ItemId -> String -> m (Maybe TagType)
updateTagById tagId name = do
    items <- queryWithExcept
        "UPDATE Tags SET Tag = ? WHERE Id = ? RETURNING * ;"
        (name , tagId)
    return $ listToMaybe items    

selectTagsWithParameters ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ) => Maybe ItemId -> Maybe String -> [( String , Maybe String )] -> m (Maybe TagType)
selectTagsWithParameters tagId name param = do
    items <- queryWithExcept
        (Query $ "SELECT id,tag FROM Tags WHERE"
            <> "(id = COALESCE (?, id))"
            <> "AND (tag = COALESCE (?, tag))"
            <> "ORDER BY tag "
            <> getLimitOffsetBS param <> ";")
        (tagId, name)
    return $ listToMaybe items    