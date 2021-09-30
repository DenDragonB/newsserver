module TestWeb where

import Test.Hspec
import qualified Data.ByteString.UTF8         as BS

import Web

param :: [( BS.ByteString , Maybe BS.ByteString )]
param = [ ("id",Just "1")
        , ("name",Just "name")
        ]



main :: IO ()
main = hspec $ do
    describe "Web functions" $ do
        it "paramToText" $ do
            let res = paramToText param
            res `shouldBe` 
                [ ("id",Just "1")
                , ("name",Just "name")
                ]