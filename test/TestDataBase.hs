{-# LANGUAGE OverloadedStrings #-}

module TestDataBase where

import           Control.Monad.Except
import           Test.Hspec

import           DataBase
import           DataBase.Posts
import           Exceptions

main :: IO ()
main = hspec $ do
    describe "DataBase functions" $ do
        it "getLimitOffsetBS exists only limit" $ do
            let res = getLimitOffsetBS [("limit",Just "2")]
            res `shouldBe` " LIMIT 2"
        it "getLimitOffsetBS exists only page" $ do
            let res = getLimitOffsetBS [("page",Just "1")]
            res `shouldBe` " OFFSET 0"
        it "getLimitOffsetBS exists limit and page" $ do
            let res = getLimitOffsetBS [("limit",Just "2"),("page",Just "1")]
            res `shouldBe` " LIMIT 2 OFFSET 0"
        it "getLimitOffsetBS not exists" $ do
            let res = getLimitOffsetBS [("name",Just "abc")]
            res `shouldBe` ""
        it "getMaybeParam exists" $ do
            let res = getMaybeParam "limit" [("limit",Just "2"),("page",Just "1")]
            res `shouldBe` Just "2"
        it "getMaybeParam not exists" $ do
            let res = getMaybeParam "name" [("limit",Just "2"),("page",Just "1")]
            res `shouldBe` Nothing
        it "getMaybeParamList exists" $ do
            let res = getMaybeParamList "tag" [("tag",Just "1,2,3"),("page",Just "1")]
            res `shouldBe` Just ["1","2","3"]
        it "getMaybeParamList not exists" $ do
            let res = getMaybeParamList "tag" [("page",Just "1")]
            res `shouldBe` Nothing
        it "parseMaybeParam exists" $ do
            res <- runExceptT $ parseMaybeParam "tag" [("tag",Just "1")]
            (res :: Either Errors (Maybe Int)) `shouldBe` Right (Just 1)
        it "parseMaybeParam exists but no parse" $ do
            res <- runExceptT $ parseMaybeParam "tag" [("tag",Just "abc")]
            (res :: Either Errors (Maybe Int)) `shouldBe` Left (ParametrParseError "tag")
        it "parseMaybeParam not exists" $ do
            res <- runExceptT $ parseMaybeParam "tag" [("name",Just "abc")]
            (res :: Either Errors (Maybe Int)) `shouldBe` Right Nothing
        it "parseMaybeParamList exists" $ do
            res <- runExceptT $ parseMaybeParamList "tag" [("tag",Just "[1,2,3]")]
            (res :: Either Errors (Maybe [Int])) `shouldBe` Right (Just [1,2,3])
        it "parseMaybeParamList exists but no parse" $ do
            res <- runExceptT $ parseMaybeParamList "tag" [("tag",Just "abc")]
            (res :: Either Errors (Maybe [Int])) `shouldBe` Left (ParametrParseError "tag")
        it "parseMaybeParamList not exists" $ do
            res <- runExceptT $ parseMaybeParamList "tag" [("name",Just "abc")]
            (res :: Either Errors (Maybe [Int])) `shouldBe` Right Nothing
        it "fromMaybeM is Just" $ do
            res <- runExceptT $ fromMaybeM NotFound (Just True)
            (res :: Either Errors Bool) `shouldBe` Right True
        it "fromMaybeM is Nothing" $ do
            res <- runExceptT $ fromMaybeM NotFound Nothing
            (res :: Either Errors Bool) `shouldBe` Left NotFound
        it "getParamM exists" $ do
            res <- runExceptT $ getParamM "name" [("name",Just "abc")]
            (res :: Either Errors String) `shouldBe` Right "abc"
        it "getParamM not exists" $ do
            res <- runExceptT $ getParamM "tag" [("name",Just "abc")]
            (res :: Either Errors String) `shouldBe` Left (WrongQueryParameter "tag")
        it "parseParamM exists" $ do
            res <- runExceptT $ parseParamM "tag" [("tag",Just "1")]
            (res :: Either Errors Int) `shouldBe` Right 1
        it "parseParamM exists but no parse" $ do
            res <- runExceptT $ parseParamM "tag" [("tag",Just "abc")]
            (res :: Either Errors Int) `shouldBe` Left (ParametrParseError "tag")
        it "parseParamM not exists" $ do
            res <- runExceptT $ parseParamM "tag" [("name",Just "abc")]
            (res :: Either Errors Int) `shouldBe` Left (WrongQueryParameter "tag")
        it "delBracketStart with [" $ do
            let res = delBracketStart "[abc"
            res `shouldBe` "abc"
        it "delBracketStart with {" $ do
            let res = delBracketStart "{abc"
            res `shouldBe` "abc"
        it "delBracketStart with no bracket" $ do
            let res = delBracketStart "abc"
            res `shouldBe` "abc"
        it "delBracketEnd with ]" $ do
            let res = delBracketEnd "abc]"
            res `shouldBe` "abc"
        it "delBracketEnd with }" $ do
            let res = delBracketEnd "abc}"
            res `shouldBe` "abc"
        it "delBracketEnd with no bracket" $ do
            let res = delBracketEnd "abc"
            res `shouldBe` "abc"
        it "isBracket Just {" $ do
            let res = isBracket (Just '{')
            res `shouldBe` True
        it "isBracket Just }" $ do
            let res = isBracket (Just '}')
            res `shouldBe` True
        it "isBracket Just [" $ do
            let res = isBracket (Just '[')
            res `shouldBe` True
        it "isBracket Just ]" $ do
            let res = isBracket (Just ']')
            res `shouldBe` True
        it "isBracket Just a" $ do
            let res = isBracket (Just 'a')
            res `shouldBe` False
        it "isBracket Nothing" $ do
            let res = isBracket Nothing
            res `shouldBe` False
        it "splitCommas with many words" $ do
            let res = splitCommas "1 2,3,4"
            res `shouldBe` ["1 2","3","4"]
        it "splitCommas with one word" $ do
            let res = splitCommas "abc"
            res `shouldBe` ["abc"]
        it "addSortBy author" $ do
            let res = addSortBy "author"
            res `shouldBe` " ORDER BY u.UserName "
        it "addSortBy date" $ do
            let res = addSortBy "date"
            res `shouldBe` " ORDER BY n.RegDate "
        it "addSortBy category" $ do
            let res = addSortBy "category"
            res `shouldBe` " ORDER BY c.CatName "
        it "addSortBy photos" $ do
            let res = addSortBy "photos"
            res `shouldBe` " ORDER BY array_length (n.Photos,1) "
        it "addSortBy something" $ do
            let res = addSortBy "abc"
            res `shouldBe` ""
