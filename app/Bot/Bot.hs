{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative              ((<|>))
import           Control.Monad.Trans              (liftIO)
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Text.Read
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import           GHC.Int (Int32)
import           Control.Monad (void)
import           Configuration.Dotenv

import           Lib
import           Model.Model as Model
import           Model.Db as Db


data Action
  = DoNothing 
  | Start -- Inital state, user will be prompt with a welcome message and a list of actions
  | BurgerMenu -- Setp one of order, choose size of burger
  | ToppingMenu (Maybe Topping) -- Setp two of order, select a topping and its size
  | SauceMenu -- Extension of setp two of order, to avoid execive amount of toppigns we sepparate souces from the rest
  | AddBurger Burger -- Saves choosed burger size to the model
  | AddTopping Topping Int -- Saves choosed toping and amount to the model
  | Order -- Adds burger to the order
  | Confirm (Maybe Client) -- Confirms order
  | Remove Text -- Removes a burger from the order
  | ShowOrder -- Shows burgers on the order
  | Help -- Shows help message to the user
	deriving (Show, Read)


commandsMessage :: Text
commandsMessage = Text.unlines
	[ "- Use /menu to choose something from the menu" 
	, "- Use /remove <number> to remove an item from your order"
 	, "- Use /show to show your order"
 	, "- Use /confirm to confirm and send your order, don't forget to review it before"
 	, ""
	]

-- | A help message to show on conversation start with bot.
startMessage :: Text
startMessage = "Hi there! Welcome to Burger Life\n\n" <> commandsMessage


orderMessage :: Burger -> Text
orderMessage burger = "Ok, added a " 
  <> ppBurgerWithPrice burger 
  <> " to your order\n\n"
  <> commandsMessage

-- | A help message to show when user input does not match any command
helpMessage :: Text
helpMessage = "I can help you with your order:\n\n" 
	<> commandsMessage


-- | A error message to show on conversation when client data is not available, so order can not start.
clientDataErrorMessage :: Text
clientDataErrorMessage = Text.unlines
  [ "We are sorry..."
  , "an error has been encountered and we can not take your order at the moment."
	, "Check that your Telegram profile is complete, we need you to fill your first name and surname to call you later when your order is ready"
  , "Please try again later!"
  , ""
  ] 


burgerMenuMessage :: Text 
burgerMenuMessage =  "Let's start your order, choose the size of your burger"


toppingMenuMessage :: Burger -> Text 
toppingMenuMessage b = "Let's add toppings to your: \n\n" 
	<> ppBurgerWithPrice b


menuBurgerKeyboard :: Telegram.InlineKeyboardMarkup
menuBurgerKeyboard = (Telegram.InlineKeyboardMarkup . map (\burger -> pure (actionButton (burgerEmoji burger <> pack (show burger) <> " $" <> pack (show (getBurgerPrice burger))) (AddBurger burger)))) burgerMenu


menuSauceKeyboard :: Telegram.InlineKeyboardMarkup
menuSauceKeyboard = Telegram.InlineKeyboardMarkup (map (\topping -> pure (actionButton (pack (show topping) <> " " <> pack (show (getToppingPrice topping) <> "$")) (AddTopping topping 1))) sauceMenu ++ [[orderBtn]])


menuToppingsInlineKeyboard :: Maybe Topping -> Telegram.InlineKeyboardMarkup
menuToppingsInlineKeyboard selected = Telegram.InlineKeyboardMarkup (
      map (toppingInlineKeyboardButton selected) toppingMenu
      ++ 
      [	[sauceBtn], [orderBtn] ]
      )


toppingInlineKeyboardButton :: Maybe Topping -> Topping -> [Telegram.InlineKeyboardButton]
toppingInlineKeyboardButton (Just selected) topping = if selected == topping then [addToppingBtn topping 1, addToppingBtn topping 2, addToppingBtn topping 3] else toppingInlineKeyboardButton Nothing topping
toppingInlineKeyboardButton Nothing topping = [actionButton (toppingEmoji topping <> pack (show topping)) (ToppingMenu (Just topping))]
  

addToppingBtn :: Topping -> Int -> Telegram.InlineKeyboardButton
addToppingBtn topping n =  actionButton (pack (show n) <> " $" <> pack (show (fromIntegral n * getToppingPrice topping))) (AddTopping topping n)


sauceBtn :: Telegram.InlineKeyboardButton
sauceBtn = actionButton "Sauce" SauceMenu


orderBtn :: Telegram.InlineKeyboardButton
orderBtn = actionButton "Order" Order


-- | tirggers previus action >>>>==== TODO ====<<<<
undoBtn :: Action -> Telegram.InlineKeyboardButton
undoBtn = actionButton "Undo"


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
   $  BurgerMenu                    <$    command "menu" 
  <|> Start                         <$    command "start" 
  <|> ShowOrder                     <$    command "show" 
  <|> Remove                        <$>   command "remove"
  <|> Confirm (getOrderData update) <$    command "confirm"
  <|> callbackQueryDataRead
  <|> Help                          <$    (command "help" <|> text)) update


-- | Handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction DoNothing model = pure model

handleAction Start model = model <# do 
      replyText startMessage
      pure DoNothing

handleAction BurgerMenu model = model <# do 
      replyOrEdit (toEditMessage burgerMenuMessage )
        {  editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup menuBurgerKeyboard }
      pure DoNothing   

handleAction (ToppingMenu selected) model = model <# do
      let Just b = currentBurger model
      editUpdateMessage (toEditMessage (toppingMenuMessage b)) 
        {  editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (menuToppingsInlineKeyboard selected) }
      pure DoNothing

handleAction SauceMenu model = model <# do
      let Just b = currentBurger model
      editUpdateMessage (toEditMessage (toppingMenuMessage b)) 
        {  editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup menuSauceKeyboard }
      pure DoNothing

handleAction (AddBurger burger) model = initOrder burger model <# do
      pure (ToppingMenu Nothing)

handleAction (AddTopping topping amount) model = fOrder (\b -> add b topping amount) model <# do
      pure (ToppingMenu Nothing)

handleAction Order model = let Just b = currentBurger model in 
      addBurger b model <# do
      editUpdateMessage (toEditMessage (orderMessage b)) 
      pure DoNothing

handleAction (Confirm (Just client)) model = Model [] Nothing  <#
		case burgers model of 
			[] -> do 
				replyText "Your order is empty. Type /menu to add a burger to your order"
				pure DoNothing
			items -> do 
				orderId <- liftIO (insertOrder client (burgers model))
				replyText ("Your order it's comming to you, order number: " <> pack (show orderId))
				pure DoNothing

handleAction (Confirm Nothing) model = model <# do
      replyText clientDataErrorMessage
      pure DoNothing      

handleAction (Remove item) model = case decimal item of 
        Left _ -> model <# do 
          replyText "Plese enter a number.\n Usage: /remove <number>"
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
