{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative              ((<|>))
import           Control.Monad.Trans              (liftIO)
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import Data.Text.Read
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

import GHC.Int (Int32)
import Lib
import Model.Model as Model
import Model.Db as Db

import Control.Monad (void)
import Configuration.Dotenv


data Action
  = DoNothing 
  | Start -- | Inital state, user will be prompt with a welcome message and a list of actions
  | MenuStepOne -- | size of burger
  | MenuStepTwo -- topping 
  | MenuStepThree Topping -- topping amount
  | MenuStepFour -- topping sauce 
  | InitOrder Burger
  | AddTopping Topping Int
  | Order
  | Place (Maybe Client)
  | Remove Text
  | ShowOrder
  | Help
  deriving (Show, Read)


-- | A help message to show on conversation start with bot.
startMessage :: Text
startMessage = Text.unlines
  [ "Hi there! Welcome to Burger Life"
  , ""
  ] 
  <> helpMessage


-- | A help message to show when user input does not match any command
helpMessage :: Text
helpMessage = Text.unlines
 [ "I can help you with your order:"
 , ""
 , "- Use /menu to choose something from the menu" -- TODO, there is an error whith buttons, add price on label??
 , "- Use /remove <number> to remove an item from your order"
 , "- Use /show to show your order"
 , "- Use /place to place your order" -- TODO
 , ""
 ]


-- | A error message to show on conversation when client data is not available, so order can not start.
clientDataErrorMessage :: Text
clientDataErrorMessage = Text.unlines
  [ "We are sorry..."
  , "an error has been encountered and we can not take your order at the moment."
  , "Please try again later!"
  , ""
  ] 


menuStepOneMessage :: Text 
menuStepOneMessage = Text.unlines
 [ "Let's start your order"
 , ""
 , "choose the size of your burger"
 ]


menuStepTwoMessage :: Text 
menuStepTwoMessage = "Let's choose toppings for your burger"


menuStepThreeMessage :: Text 
menuStepThreeMessage = "Let's choose the amount for your topping"


menuBurgerKeyboard :: Telegram.InlineKeyboardMarkup
menuBurgerKeyboard = (Telegram.InlineKeyboardMarkup . map (\burger -> pure (actionButton (pack (show burger) <> " $" <> pack (show (getBurgerPrice burger))) (InitOrder burger)))) burgerMenu


menuToppingsKeyboard :: Telegram.InlineKeyboardMarkup
menuToppingsKeyboard = Telegram.InlineKeyboardMarkup (map (\topping -> pure (actionButton (pack (show topping)) (MenuStepThree topping))) toppingMenu ++ [[sauceBtn], [orderBtn]])


menuSauceKeyboard :: Telegram.InlineKeyboardMarkup
menuSauceKeyboard = Telegram.InlineKeyboardMarkup (map (\topping -> pure (actionButton (pack (show topping) <> " " <> pack (show (getToppingPrice topping) <> "$")) (AddTopping topping 1))) sauceMenu ++ [[orderBtn]])


menuAmountKeyboard :: Topping -> Telegram.InlineKeyboardMarkup
menuAmountKeyboard topping = Telegram.InlineKeyboardMarkup [[btnAdd 1, btnAdd 2, btnAdd 3]] where
   btnAdd n =  actionButton (pack (show n) <> " $" <> pack (show (fromIntegral n * getToppingPrice topping))) (AddTopping topping n)


sauceBtn :: Telegram.InlineKeyboardButton
sauceBtn = actionButton "Sauce" MenuStepFour


orderBtn :: Telegram.InlineKeyboardButton
orderBtn = actionButton "Order" Order


-- | Auxiliar function to retrive message sender data from telegram-bot-simple library. 
-- This allows us to know to whom (client) we are speaking 
getOrderData :: Telegram.Update -> Maybe Client
getOrderData update =  do 
  msg <- Telegram.updateMessage update
  user <- Telegram.messageFrom msg
  lastName <- Telegram.userLastName user
  return (Client { Model.clientId = getId user, clientFirstName = Telegram.userFirstName user, clientLastName = lastName, Model.date = posixSecondsToUTCTime (Telegram.messageDate msg)})


-- | Process incoming 'Telegram.Update's and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _model update = (parseUpdate
   $ MenuStepOne                    <$  command "menu"
  <|> Start                         <$  command "start"
  <|> ShowOrder                     <$  command "show" 
  <|> Remove                        <$>  command "remove"
  <|> Place (getOrderData update)   <$  command "place"
  <|> callbackQueryDataRead
  <|> Help                          <$ text) update


-- | Handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction DoNothing model = pure model

handleAction Start model = model <# do 
      replyText startMessage
      pure DoNothing

handleAction MenuStepOne model = model <# do
      reply (toReplyMessage menuStepOneMessage) 
        { replyMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup menuBurgerKeyboard }
      pure DoNothing

handleAction MenuStepTwo model = model <# do
      reply (toReplyMessage menuStepTwoMessage) 
        { replyMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup menuToppingsKeyboard }
      pure DoNothing

handleAction (MenuStepThree topping) model = model <# do
      reply (toReplyMessage menuStepThreeMessage) 
        { replyMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (menuAmountKeyboard topping) }
      pure DoNothing 

handleAction MenuStepFour model = model <# do
      reply (toReplyMessage menuStepTwoMessage) 
        { replyMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup menuSauceKeyboard }
      pure DoNothing

handleAction (InitOrder burger) model = initOrder burger model <# do
      pure MenuStepTwo

handleAction (AddTopping topping amount) model = let Just burger = currentBurger model in fOrder (\b -> add b topping amount) model <# do
      let burgerWithTopping = add burger topping amount
      replyText ("Ok. " <> ppBurger burgerWithTopping)
      pure MenuStepTwo

handleAction Order model = let Just b = currentBurger model in 
      addBurger b model <# do
      pure Start

handleAction (Place (Just client)) model = model <# do 
      orderId <- liftIO (insertOrder client (burgers model))
      replyText ("Your order it's comming to you, order number: " <> pack (show orderId))
      pure DoNothing

handleAction (Place Nothing) model = model <# do
      replyText clientDataErrorMessage
      pure DoNothing      

handleAction (Remove item) model = case decimal item of 
        Left _ -> model <# do 
          replyText "Plese enter a number"
          pure DoNothing    
        Right (index,_) -> removeBurger (index-1) model <# do
          replyText "removed"
          pure ShowOrder

handleAction ShowOrder model = model <# do 
      replyText (ppOrder model)
      pure DoNothing

handleAction Help model = model <# do 
      replyText helpMessage
      pure DoNothing


-- | Bot Application
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model [] Nothing 
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }


-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault (conversationBot Telegram.updateChatId bot)) env


-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = do 
      void $ loadFile defaultConfig
      token <- getEnvToken "TOKEN_TELEGRAM"
      run token
      return ()
