module TestLogger where

import           Test.Hspec

import           Logger

config :: Config
config = Config
    LogToConsole
    ""
    Info

main :: IO ()
main = hspec $ do
    describe "Logger functions" $ do
        it "logToConsole" $ do
            res <- logToConsole Info "abc"
            res `shouldBe` ()
        it "log with upper then logMinLevel" $ do
            res <- Logger.log config Error "abc"
            res `shouldBe` ()
        it "log with lower then logMinLevel" $ do
            res <- Logger.log config Debug "abc"
            res `shouldBe` ()
