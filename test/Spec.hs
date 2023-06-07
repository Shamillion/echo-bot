import Config
  ( Configuration (..),
    Priority (ERROR),
  )
import Control.Monad.Identity
  ( Identity,
    runIdentity,
  )
import Control.Monad.State.Lazy
  ( evalStateT,
    execStateT,
  )
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Environment
  ( Environment (..),
    NumRepeats (..),
    UpdateID (..),
    Username (Username),
  )
import Lib
  ( Command (..),
    WorkHandle (..),
    ifKeyWord,
    wordIsRepeat,
  )
import Telegram
  ( Chat (..),
    Message (..),
    MessageDate (..),
    WholeObject (..),
  )
import Test.Hspec
  ( SpecWith,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Test.QuickCheck (verbose)

environmentT :: Environment
environmentT = Environment 0 $ Map.singleton "" (NumRepeats 3)

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
      helpMess = ["help message"],
      repeatMess = "repeat message",
      defaultRepeats = 3,
      priorityLevel = ERROR,
      logOutput = "cons"
    }

-- Handle for tests of echobot.
handlerForTestIfKeyWord :: WorkHandle Identity String String
handlerForTestIfKeyWord =
  WorkHandle
    { writingLineH = \_ _ -> pure "writingLine",
      sendKeyboardH = \_ _ -> pure "keyboard",
      sendCommentH = \_ _ -> pure "comment",
      sendRepeatsH = \_ _ -> pure "sendRepeats",
      wordIsRepeatH = \_ _ _ _ -> pure $ Report "/repeat",
      currentMessengerH = pure "no matter",
      configurationH = pure testConfig,
      getDataH = pure Nothing
    }

-- Handle for tests of echobot.
handlerForTestWordIsRepeat :: WorkHandle Identity String String
handlerForTestWordIsRepeat =
  handlerForTestIfKeyWord
    { wordIsRepeatH = \_ _ _ _ -> pure $ Report "empty array"
    }

nothing :: UpdateID -> Identity (Maybe WholeObject)
nothing _ = pure Nothing

messageDate :: T.Text -> T.Text -> MessageDate
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

testingFunctionIfKeyWord :: T.Text -> Command
testingFunctionIfKeyWord txt =
  runIdentity $
    evalStateT (ifKeyWord handlerForTestIfKeyWord nothing $ messageDate "testUsr" txt) environmentT

testingFunctionWordIsRepeat :: T.Text -> T.Text -> T.Text -> Command
testingFunctionWordIsRepeat usr1 usr2 txt =
  runIdentity $
    evalStateT
      ( wordIsRepeat
          handlerForTestWordIsRepeat
          nothing
          (messageDate usr1 txt)
          [messageDate usr2 txt]
      )
      environmentT

testingFunctionWordIsRepeat' :: T.Text -> T.Text -> T.Text -> Maybe NumRepeats
testingFunctionWordIsRepeat' usr1 usr2 txt =
  Map.lookup (Username usr1) $
    userData $
      runIdentity $
        execStateT
          ( wordIsRepeat
              handlerForTestWordIsRepeat
              nothing
              (messageDate usr1 txt)
              [messageDate usr2 txt]
          )
          environmentT

testingFunctionWordIsRepeatWithEmptyArr :: T.Text -> T.Text -> Command
testingFunctionWordIsRepeatWithEmptyArr usr1 txt =
  runIdentity $
    evalStateT
      ( wordIsRepeat
          handlerForTestWordIsRepeat
          nothing
          (messageDate usr1 txt)
          []
      )
      environmentT

testsFunctionIfKeyWord :: SpecWith ()
testsFunctionIfKeyWord = do
  it "catch the /help" $
    testingFunctionIfKeyWord "/help" `shouldBe` Help

  it "catch the /repeat" $
    testingFunctionIfKeyWord "/repeat" `shouldBe` Report "/repeat"

  it "catch the others" $
    verbose $ \x xs -> testingFunctionIfKeyWord (T.pack (x : xs)) == Report "not a keyword"

testsFunctionWordIsRepeat :: SpecWith ()
testsFunctionWordIsRepeat = do
  it "catch the number from 1 to 5" $ do
    testingFunctionWordIsRepeat "Tony" "Tony" "1" `shouldBe` Repeat 1
    testingFunctionWordIsRepeat "Tony" "Tony" "3" `shouldBe` Repeat 3
    testingFunctionWordIsRepeat "Tony" "Tony" "5" `shouldBe` Repeat 5

    testingFunctionWordIsRepeat' "Tony" "Tony" "1" `shouldBe` Just (NumRepeats 1)
    testingFunctionWordIsRepeat' "Tony" "Tony" "3" `shouldBe` Just (NumRepeats 3)
    testingFunctionWordIsRepeat' "Tony" "Tony" "5" `shouldBe` Just (NumRepeats 5)

  it "the number is not from 1 to 5" $ do
    testingFunctionWordIsRepeat "Tony" "Tony" "6" `shouldBe` Report "number out of range"
    testingFunctionWordIsRepeat' "Tony" "Tony" "6" `shouldBe` Nothing

  it "another user" $ do
    testingFunctionWordIsRepeat' "Tony" "Many" "3" `shouldBe` Nothing
    testingFunctionWordIsRepeat' "Tony" "Many" "6" `shouldBe` Nothing

  it "empty array" $
    testingFunctionWordIsRepeatWithEmptyArr "Tony" "3" `shouldBe` Report "empty array"

main :: IO ()
main = hspec $ do
  describe "Test function ifKeyWord" testsFunctionIfKeyWord
  describe "Test function wordIsRepeat" testsFunctionWordIsRepeat
