{-# LANGUAGE OverloadedStrings #-}

module Model.BotModel where

import           GHC.Int (Int32)
import           Data.Text                        (Text, pack)
import Data.Time ( UTCTime )
import Control.Monad.State ( modify, MonadState(state), State )

import Lib ( takeAt, toTouple, withNewLine )
import Model.Burger
    ( Prices, Topping, Burger, ppBurgerWithPrice, add, orderPrice )


data User = User { 
  userId    :: Int32, 
  firstName :: Text,
  lastName  :: Text 
} deriving (Show)


data BotModel = BotModel { 
  burgers         :: [Burger], -- burgers added to order
  currentBurger   :: Maybe Burger, -- burger being created
  prices          :: Prices, -- actual prices 
  actions         :: [Burger] -- stack of steps performed to create currentBurger
} deriving (Show)


data Client = Client { 
  clientId        :: Int32,
  clientFirstName :: Text,
  clientLastName  :: Text,
  date            :: UTCTime
} deriving (Show, Read)  


-- | Pretty print order 
ppOrder :: Prices -> [Burger] -> Text
ppOrder prices burgers = case foldMap (withNewLine . (\(b, i) -> pack (show i) <> ". " <> ppBurgerWithPrice prices b)) (zipWith toTouple burgers [1..]) of
    ""    -> "Your order is empty. Type /menu to add a burger to your order"
    items -> "Your order:\n" <> items <> "\nTotal: $" <> pack (show (orderPrice prices burgers))


-- | Clears order state, for the next one
-- Returns resulting model as value of state computation
emptyOrder :: State BotModel BotModel
emptyOrder = state (\model -> let model' = model { currentBurger = Nothing, burgers = [], actions = []} in (model', model'))


-- | Aux function, saves burger as currentBurger
-- Returns resulting burger as value of state computation
setCurrentBurger :: Burger -> State BotModel Burger
setCurrentBurger burger = state (\model -> (burger , model {actions = [], currentBurger = Just burger }))


-- | Saves burger as current for costumization, add's first step to actions stack
-- Returns resulting burger as value of state computation
initBurger :: Burger -> State BotModel Burger
initBurger burger = do 
    setCurrentBurger burger
    push burger
    return burger


-- | Aux function, add's topping to currentBurger
-- Returns resulting burger as value of state computation
addToppingToCurrentBurger :: Topping -> Int -> State BotModel (Maybe Burger)
addToppingToCurrentBurger topping amount = state (\model -> let b' = h (currentBurger model) in (b', model { currentBurger = b'})) where
    h Nothing = Nothing
    h (Just b) = Just $ add b topping amount


-- | Add topping to currentBurger, add's resulting burger to actions stack. 
-- Returns resulting burger as value of state computation
addTopping :: Topping -> Int -> State BotModel (Maybe Burger)    
addTopping topping amount = do 
    burger <- addToppingToCurrentBurger topping amount
    case burger of 
        Nothing -> return Nothing
        Just b  -> do 
            push b
            return burger


-- | Performes undo operation: pop's from the stack prev action performed and saves it as currentBurger
-- Returns the prev burger as value of state computation
undo :: State BotModel (Maybe Burger)
undo = do 
    burger <- pop
    modify (\model -> model {currentBurger = if null (actions model) 
                                                then Nothing 
                                                else Just $ actions model !! (length (actions model) -1)} )
    case burger of 
        Nothing -> return Nothing
        Just b  -> return burger


-- | Saves burger to order
-- Returns burger as value of state computation
addToOrder :: Burger -> State BotModel Burger
addToOrder burger = state $ \model -> (burger, model {burgers = burgers model ++ [burger] })


-- | Removes burger at index from order
-- Returns removed burger as value of state computation
removeFromOrder :: Int -> State BotModel Burger
removeFromOrder index = state $ \model -> (burgers model !! index, model { burgers = takeAt index (burgers model) })


-- | Pop's burger from actions stack
-- Returns burger from resulting operation as value of state computation
pop :: State BotModel (Maybe Burger)
pop = state (\model -> let (burger, actions') = h (actions model) in (burger, model {actions = actions'}) ) where
    h [] = (Nothing, [])
    h xs = (Just $ xs !! (length xs -1), take (length xs - 1) xs)


-- | Pushes burger to actions stack
-- Returns burger from resulting operation as value of state computation
push :: Burger -> State BotModel Burger
push action = state (\model -> (action, model {actions = actions model ++ [action]}))
