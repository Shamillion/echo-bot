{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Identity
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State.Lazy
import Network.HTTP.Simple
import Lib
-- import Control.Exception (evaluate)

testHandler :: WorkHandle Identity String      -- Handle for tests of echobot
testHandler  = WorkHandle
  { writingLine'  = \x y -> pure "nothing"
  , sendKeyboard' = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sendComment'  = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sandRepeats'  = \x y -> pure "nothing"
  , wordIsRepeat' = \x y z -> pure "/repeat"
  , pureOne = pure "/help" 
  , pureTwo = pure "anything"  
  }
  
nothing :: Int -> Maybe WholeObject  
nothing num = Nothing  

messageDate :: T.Text -> MessageDate  
messageDate txt = MessageDate
  { update_id = 101
  , message = Message                
               { message_id = 202
               , chat = Chat                     
                         { Lib.id = 303
                         , username = "userTest" 
                         }
               , textM = Just txt
               }    
  }

testingFunctionIfKeyWord :: T.Text -> String
testingFunctionIfKeyWord txt = 
  runIdentity $ 
    evalStateT (ifKeyWord testHandler nothing  (messageDate txt)) environment 

      
main :: IO ()
main = hspec $ do
  describe "Test function ifKeyWord" $ do
    it "catch the /help" $ 
      testingFunctionIfKeyWord "/help" `shouldBe` "/help"
      
    it "catch the /repeat" $ 
      testingFunctionIfKeyWord "/repeat" `shouldBe` "/repeat"  
      
    it "catch the others" $ 
      verbose $ \x xs -> testingFunctionIfKeyWord (T.pack (x:xs)) == "anything"    

    --it "returns the first element of an *arbitrary* list" $
      --verbose $ \x xs -> head (x:xs) == (x :: Int)      
