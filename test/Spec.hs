import Config (Configuration (..))
import Control.Monad.Identity
  ( Identity,
    runIdentity,
  )
import Control.Monad.State.Lazy (evalStateT)
import Data
  ( Chat (..),
    Message (..),
    MessageDate (..),
  )
import qualified Data.Map.Lazy as Map
import Environment
  ( Environment (..),
    NumRepeats (..),
  )
import Lib
  ( Command (..),
    WorkHandle (..),
    handleKeywords,
    handleRepeatCommand,
  )
import Logger.Data (Priority (ERROR))
import Test.Hspec
  ( SpecWith,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Test.QuickCheck (verbose)

environmentT :: Environment
environmentT = Environment 0 (Map.singleton "" (NumRepeats 3)) testConfig

testConfig :: Configuration
testConfig =
  Configuration
    { messenger = "no matter",
      hostTG = "0",
      hostVK = "0",
      tokenTG = "0",
      tokenVK = "0",
      groupIdVK = 0,
      apiVKVersion = "0",
      messageHelpCommand = ["help message"],
      messageRepeatCommand = "repeat message",
      defaultRepeats = 3,
      priorityLevel = ERROR,
      logOutput = "cons"
    }

handlerForTest :: WorkHandle Identity String String
handlerForTest =
  WorkHandle
    { writingLineH = \_ _ -> pure "writingLine",
      getDataH = pure Nothing,
      addNumberH = 0,
      stringForCreateKeyboardH = \_ _ -> "stringForCreateKeyboard",
      stringCommentH = \_ _ -> "stringComment",
      repeatMessageH = \_ -> pure "repeatMessage",
      sendHttpReqH = \_ -> pure "sendHttpReq",
      repeatActionH = \n _ -> pure $ show n
    }

messageDate :: String -> String -> MessageDate
messageDate usr txt =
  MessageDate
    { update_id = 101,
      message =
        Message
          { message_id = 202,
            chat =
              Chat
                { chat_id = 303,
                  username = usr
                },
            textM = Just txt,
            attachments = Nothing
          }
    }

testingFunctionHandleKeywords :: String -> Command
testingFunctionHandleKeywords txt =
  runIdentity $
    evalStateT (handleKeywords handlerForTest $ messageDate "testUsr" txt) environmentT

testingFunctionHandleRepeatCommand :: String -> String -> String -> Command
testingFunctionHandleRepeatCommand usr1 usr2 txt =
  runIdentity $
    evalStateT
      ( handleRepeatCommand
          handlerForTest
          (messageDate usr1 txt)
          [messageDate usr2 txt]
      )
      environmentT

testingFunctionHandleRepeatCommandWithEmptyArr :: String -> String -> Command
testingFunctionHandleRepeatCommandWithEmptyArr usr1 txt =
  runIdentity $
    evalStateT
      ( handleRepeatCommand
          handlerForTest
          (messageDate usr1 txt)
          []
      )
      environmentT

testsFunctionhandleKeywords :: SpecWith ()
testsFunctionhandleKeywords = do
  it "catch the /help" $
    testingFunctionHandleKeywords "/help" `shouldBe` Help

  it "catch the /repeat" $
    testingFunctionHandleKeywords "/repeat" `shouldBe` Report "Repeat"

  it "catch the others" $
    verbose $ \x xs -> testingFunctionHandleKeywords (x : xs) == Report "not a keyword"

testsFunctionhandleRepeatCommand :: SpecWith ()
testsFunctionhandleRepeatCommand = do
  it "catch the number from 1 to 5" $ do
    testingFunctionHandleRepeatCommand "Tony" "Tony" "1" `shouldBe` Repeat 1
    testingFunctionHandleRepeatCommand "Tony" "Tony" "3" `shouldBe` Repeat 3
    testingFunctionHandleRepeatCommand "Tony" "Tony" "5" `shouldBe` Repeat 5

  it "the number is not from 1 to 5" $ do
    testingFunctionHandleRepeatCommand "Tony" "Tony" "6" `shouldBe` Report "number out of range"
    testingFunctionHandleRepeatCommand "Tony" "Tony" "0" `shouldBe` Report "number out of range"

  it "another user" $ do
    testingFunctionHandleRepeatCommand "Tony" "Many" "3" `shouldBe` Report "another user"
    testingFunctionHandleRepeatCommand "Tony" "Many" "6" `shouldBe` Report "another user"

  it "empty array" $
    testingFunctionHandleRepeatCommandWithEmptyArr "Tony" "3" `shouldBe` Report "empty array"

main :: IO ()
main = hspec $ do
  describe "Test function handleKeywords" testsFunctionhandleKeywords
  describe "Test function handleRepeatCommand" testsFunctionhandleRepeatCommand
