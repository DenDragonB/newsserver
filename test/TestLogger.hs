module TestLogger where

import Test.Hspec

import Logger

main :: IO ()
main = hspec $ do
    describe "Logger functions" $ do
        it "Cant test enything" True