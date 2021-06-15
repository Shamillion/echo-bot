import Test.Hspec
import Test.QuickCheck
import Control.Monad.Identity
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Simple
import Lib
-- import Control.Exception (evaluate)

testHandler :: WorkHandle Identity String
testHandler  = WorkHandle
  { writingLine'  = \x y -> pure ()
  , sendKeyboard' = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sendComment'  = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sandRepeats'  = \x y -> pure ()
  , pure' = pure   
  }

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      verbose $ \x xs -> head (x:xs) == (x :: Int)
