{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Identity
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State.Lazy
import Network.HTTP.Simple
import Lib
-- import Control.Exception (evaluate)

handlerForTestIfKeyWord :: WorkHandle Identity String      -- Handle for tests of echobot
handlerForTestIfKeyWord  = WorkHandle
  { writingLine'  = \x y -> pure "nothing"
  , sendKeyboard' = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sendComment'  = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sandRepeats'  = \x y -> pure "nothing"
  , wordIsRepeat' = \w x y z -> pure "/repeat"
  , pureOne = pure "/help" 
  , pureTwo = pure "anything"  
  }
  
handlerForTestWordIsRepeat :: WorkHandle Identity String      -- Handle for tests of echobot
handlerForTestWordIsRepeat  = WorkHandle
  { writingLine'  = \x y -> pure "nothing"
  , sendKeyboard' = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sendComment'  = \x y -> pure $ unsafePerformIO $ httpLBS $ parseRequest_ ""
  , sandRepeats'  = \x y -> pure "nothing"
  , wordIsRepeat' = \w x y z -> pure "Another user || empty array"
  , pureOne = pure "Number from 1 to 5" 
  , pureTwo = pure "Not number from 1 to 5"  
  }  
  
nothing :: Int -> Maybe WholeObject  
nothing num = Nothing  

messageDate :: T.Text -> T.Text -> MessageDate  
messageDate usr txt = MessageDate
  { update_id = 101
  , message = Message                
               { message_id = 202
               , chat = Chat                     
                         { Lib.id = 303
                         , username = usr 
                         }
               , textM = Just txt
               }    
  }
  
--testEnvironment :: Environment  
--testEnvironment =  Environment 0 $ Map.singleton "userTest" (defaultRepaets configuration)   

testingFunctionIfKeyWord :: T.Text -> String
testingFunctionIfKeyWord txt = 
  runIdentity $ 
    evalStateT (ifKeyWord handlerForTestIfKeyWord nothing $ messageDate "testUsr" txt) environment 

testingFunctionWordIsRepeat :: T.Text -> T.Text -> T.Text -> String
testingFunctionWordIsRepeat usr1 usr2 txt = 
  runIdentity $ evalStateT 
    (wordIsRepeat handlerForTestWordIsRepeat 
      nothing (messageDate usr1 txt) [messageDate usr2 txt]) environment 
      
testingFunctionWordIsRepeat' :: T.Text -> T.Text -> T.Text -> Maybe NumRepeats
testingFunctionWordIsRepeat' usr1 usr2 txt = 
  Map.lookup usr1 $ userData $ runIdentity $ execStateT 
    (wordIsRepeat handlerForTestWordIsRepeat 
      nothing (messageDate usr1 txt) [messageDate usr2 txt]) environment 
      
testingFunctionWordIsRepeatWithEmptyArr :: T.Text -> T.Text -> String
testingFunctionWordIsRepeatWithEmptyArr usr1 txt = 
  runIdentity $ evalStateT 
    (wordIsRepeat handlerForTestWordIsRepeat 
      nothing (messageDate usr1 txt) []) environment            


      
main :: IO ()
main = hspec $ do
  describe "Test function ifKeyWord" $ do
    it "catch the /help" $ 
      testingFunctionIfKeyWord "/help" `shouldBe` "/help"
      
    it "catch the /repeat" $ 
      testingFunctionIfKeyWord "/repeat" `shouldBe` "/repeat"  
      
    --it "catch the others" $ 
      --verbose $ \x xs -> testingFunctionIfKeyWord (T.pack (x:xs)) == "anything"    

  describe "Test function wordIsRepeat" $ do    
    it "catch the number from 1 to 5" $ do
      testingFunctionWordIsRepeat "Tony" "Tony" "1" `shouldBe` "Number from 1 to 5"
      testingFunctionWordIsRepeat "Tony" "Tony" "3" `shouldBe` "Number from 1 to 5"       
      testingFunctionWordIsRepeat "Tony" "Tony" "5" `shouldBe` "Number from 1 to 5" 
      
      testingFunctionWordIsRepeat' "Tony" "Tony" "1" `shouldBe` Just 1
      testingFunctionWordIsRepeat' "Tony" "Tony" "3" `shouldBe` Just 3
      testingFunctionWordIsRepeat' "Tony" "Tony" "5" `shouldBe` Just 5      
      
    it "the number is not from 1 to 5" $ do
      testingFunctionWordIsRepeat  "Tony" "Tony" "6" `shouldBe` "Not number from 1 to 5"
      testingFunctionWordIsRepeat' "Tony" "Tony" "6" `shouldBe` Nothing              

    it "another user" $ do
      testingFunctionWordIsRepeat' "Tony" "Many" "3" `shouldBe` Nothing
      testingFunctionWordIsRepeat' "Tony" "Many" "6" `shouldBe` Nothing
    
    it "empty array" $  
      testingFunctionWordIsRepeatWithEmptyArr "Tony" "3" `shouldBe` "Another user || empty array"
   
     

  
              
