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
import           Control.Monad.State
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import           GHC.Int (Int32)
import           Control.Monad (void)
import           Configuration.Dotenv

import           Lib
import           Model.Burger
import           Model.BotModel as BotModel
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
  | Undo -- Undo's an action of AddTopping or AddBurger
    deriving (Show, Read)


commandsMessage :: Text
commandsMessage = Text.unlines
	[ "- Use /start to start a new order" 
  , "- Use /menu to choose something from the menu" 
	, "- Use /remove <number> to remove an item from your order"
 	, "- Use /show to show your order"
 	, "- Use /confirm to confirm and send your order, don't forget to review it before"
 	, ""
	]


-- | A help message to show on conversation start with bot.
startMessage :: Text
startMessage = "Hi there! Welcome to Burger Bot\n\n" <> commandsMessage


orderMessage :: Prices -> Burger -> Text
orderMessage prices burger = "Ok, added a " 
  <> ppBurgerWithPrice prices burger 
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


toppingMenuMessage :: Prices -> Burger -> Text 
toppingMenuMessage prices b = "Let's add toppings to your: \n\n" 
	<> ppBurgerWithPrice prices b


menuBurgerInlineKeyboard :: Prices -> Telegram.InlineKeyboardMarkup
menuBurgerInlineKeyboard prices = Telegram.InlineKeyboardMarkup (map (return . addBurgerBtn prices) burgerMenu)


menuSauceInlineKeyboard :: Prices -> Telegram.InlineKeyboardMarkup
menuSauceInlineKeyboard prices = Telegram.InlineKeyboardMarkup (map (return . flip (addSauceBtn prices) 1) sauceMenu ++ [[orderBtn]])


menuToppingsInlineKeyboard :: Prices -> Maybe Topping -> Telegram.InlineKeyboardMarkup
menuToppingsInlineKeyboard prices selected = Telegram.InlineKeyboardMarkup (
      map (toppingInlineKeyboardButton prices selected) toppingMenu
      ++ 
      [	[sauceBtn], [undoBtn], [orderBtn] ]
      )


toppingInlineKeyboardButton :: Prices -> Maybe Topping -> Topping -> [Telegram.InlineKeyboardButton]
toppingInlineKeyboardButton prices (Just selected) topping = if selected == topping then [addToppingBtn prices topping 1, addToppingBtn prices topping 2, addToppingBtn prices topping 3] else toppingInlineKeyboardButton prices Nothing topping
toppingInlineKeyboardButton prices Nothing topping = [actionButton (toppingEmoji topping <> pack (show topping)) (ToppingMenu (Just topping))]


addBurgerBtn :: Prices -> Burger -> Telegram.InlineKeyboardButton
addBurgerBtn prices burger = actionButton (burgerEmoji burger <> pack (show burger) <> " $" <> pack (show (getBurgerPrice prices burger))) (AddBurger burger)


addToppingBtn :: Prices -> Topping -> Int -> Telegram.InlineKeyboardButton
addToppingBtn prices topping n = actionButton (pack (show n) <> " $" <> pack (show (fromIntegral n * getToppingPrice prices topping))) (AddTopping topping n)

addSauceBtn :: Prices -> Topping -> Int -> Telegram.InlineKeyboardButton
addSauceBtn prices topping n = actionButton (pack (show topping) <> " $" <> pack (show (fromIntegral n * getToppingPrice prices topping))) (AddTopping topping n)

sauceBtn :: Telegram.InlineKeyboardButton
sauceBtn = actionButton "Sauce" SauceMenu


orderBtn :: Telegram.InlineKeyboardButton
orderBtn = actionButton "Order" Order


-- | tirggers previus action >>>>==== TODO ====<<<<
undoBtn :: Telegram.InlineKeyboardButton
undoBtn = actionButton "Undo" Undo


-- | Auxiliar function to retrive message sender data from telegram-bot-simple library. 
-- This allows us to know to whom (client) we are speaking 
getOrderData :: Telegram.Update -> Maybe Client
getOrderData update =  do 
  msg       <- Telegram.updateMessage update
  user      <- Telegram.messageFrom msg
  lastName  <- Telegram.userLastName user
  return (Client { BotModel.clientId = getId user, clientFirstName = Telegram.userFirstName user, clientLastName = lastName, BotModel.date = posixSecondsToUTCTime (Telegram.messageDate msg)})


-- | Process incoming 'Telegram.Update's and turn them into 'Action's.
handleUpdate :: BotModel ->  Telegram.Update -> Maybe Action
handleUpdate _model update = (parseUpdate
   $  BurgerMenu                    <$    command "menu" 
  <|> Start                         <$    command "start" 
  <|> ShowOrder                     <$    command "show" 
  <|> Remove                        <$>   command "remove"
  <|> Confirm (getOrderData update) <$    command "confirm"
  <|> callbackQueryDataRead
  <|> Help                          <$    (command "help" <|> text)) update


-- | Handle 'Action's.
handleAction :: Action -> BotModel ->  Eff Action BotModel
handleAction DoNothing model = return model

handleAction Start model = execState emptyOrder model <# do 
      replyText startMessage
      return DoNothing

handleAction BurgerMenu model = model <# do 
      replyOrEdit (toEditMessage burgerMenuMessage )
        {  editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (menuBurgerInlineKeyboard (prices model))}
      return DoNothing   

handleAction (ToppingMenu selected) model = case currentBurger model of 
      Nothing   ->  model <# do 
        return BurgerMenu
      (Just b)  ->  model <# do 
        editUpdateMessage (toEditMessage (toppingMenuMessage (prices model) b)) 
          {  editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (menuToppingsInlineKeyboard (prices model) selected) }
        return DoNothing

handleAction SauceMenu model = case currentBurger model of 
      Nothing   ->  model <# do
        return BurgerMenu
      (Just b)  ->  model <# do
        editUpdateMessage (toEditMessage (toppingMenuMessage (prices model) b)) 
          {  editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (menuSauceInlineKeyboard (prices model))}
        return DoNothing

handleAction (AddBurger burger) model = execState (initBurger burger) model <# do
      return (ToppingMenu Nothing)

handleAction (AddTopping topping amount) model = execState (addTopping topping amount) model <# do
      return (ToppingMenu Nothing)

handleAction Order model = case currentBurger model of 
      Nothing   ->  model <# do
        return BurgerMenu
      (Just b)  ->  execState (addToOrder b) model <# do
        editUpdateMessage (toEditMessage (orderMessage (prices model) b)) 
        return DoNothing

handleAction (Confirm (Just client)) model = execState emptyOrder model <#
		case burgers model of 
			[]    -> do 
				replyText "Your order is empty. Type /menu to add a burger to your order"
				return DoNothing
			items -> do 
				orderId <- liftIO (insertOrder client (map (getPrice (prices model)) (burgers model)) (burgers model))
				replyText ("Your order it's comming to you!!\n\n" <> ppOrder (prices model) (burgers model) <> "\nOrder number: " <> pack (show orderId))
				return DoNothing

handleAction (Confirm Nothing) model = model <# do
      replyText clientDataErrorMessage
      return DoNothing      

handleAction (Remove item) model = case decimal item of 
        Left _          -> model <# do 
          replyText "Plese enter a number.\nUsage: /remove <number>"
          return DoNothing    
        Right (index,_) -> execState (removeFromOrder (index-1)) model <# do
          replyText "Removed"
          return ShowOrder

handleAction ShowOrder model = model <# do 
      replyText (ppOrder (prices model) (burgers model))
      return DoNothing

handleAction Help model = model <# do 
      replyText helpMessage
      return DoNothing   

handleAction Undo model = let (burger, model') = runState undo model in
    case burger of 
      Just Layer {} -> model' <# return (ToppingMenu Nothing)
      _ ->  model' <# return BurgerMenu


-- | Bot Application
getBot :: IO (BotApp BotModel Action)
getBot = do
  burgersPrices <- selectBurgersPrices
  toppingsPrices <- selectToppingsPrices
  let bot = BotApp  { botInitialModel = BotModel [] Nothing (Prices toppingsPrices burgersPrices) []
                    , botAction = flip handleUpdate
                    , botHandler = handleAction
                    , botJobs = []  }
  return bot                    


-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  bot <- getBot
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault (conversationBot Telegram.updateChatId bot)) env


-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = do 
      void $ loadFile defaultConfig
      token <- getEnvToken "TOKEN_TELEGRAM"
      run token
      return ()
