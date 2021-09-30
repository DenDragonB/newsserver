module TestDataBase where

import Test.Hspec

import DataBase

main :: IO ()
main = hspec $ do
    describe "DataBase functions" $ do
        it "Cant test enything" $ do
            let res = True
            res `shouldBe` True