{-# LANGUAGE OverloadedStrings #-}

module Model.Model where
    
import           GHC.Int (Int32)
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import Lib

data User = User { 
  userId    :: Int32, 
  firstName :: Text,
  lastName  :: Text 
} deriving (Show)

data Prices = Prices {
  toppingsPrices  :: [(Topping, Double)],
  burgersPrices   :: [(Burger, Double)]
} deriving (Show)

data Model = Model { 
  burgers         :: [Burger],
  currentBurger   :: Maybe Burger,
  prices          :: Prices
} deriving (Show)


data Client = Client { 
  clientId        :: Int32,
  clientFirstName :: Text,
  clientLastName  :: Text,
  date            :: UTCTime
} deriving (Show, Read)


initOrder :: Burger -> Model -> Model
initOrder burger model = model { currentBurger = Just burger }


changeOrder :: Burger -> Model -> Model
changeOrder b model = model { currentBurger = Just b }


fOrder :: (Burger -> Burger) -> Model -> Model
fOrder f model =  model { currentBurger = case currentBurger model of 
  Nothing -> Nothing
  Just b -> Just (f b) } 


-- | Adds a burger from the menu to client order
addBurger :: Burger -> Model -> Model
addBurger burger model =  model { burgers = burgers model ++ [burger] }


-- | removes the burger at index from the menu to client order
removeBurger :: Int -> Model -> Model
removeBurger index model =  model { burgers = takeAt index (burgers model) }


withNewLine :: Text -> Text
withNewLine = ( <> "\n")


ppOrder :: Model -> Text
ppOrder model = case foldMap (withNewLine . (\(b, i) -> pack (show i) <> ". " <> ppBurgerWithPrice (prices model) b)) (zipWith toTouple (burgers model) [1..]) of
    ""    -> "Your order is empty. Type /menu to add a burger to your order"
    items -> "Your order:\n" <> items <> "\nTotal: $" <> pack (show (orderPrice model))


data Burger =  Layer Int Topping Burger | Simple | Double | Triple | Empty deriving (Show, Eq, Read)
data Topping = Tomato | Cheese | Egg | Onion | Bacon |Lettuce | Pickle | Mushroom | Mayo | Ketchup | Mustard  deriving (Show, Eq, Read)


ppBurger :: Burger -> Text
ppBurger Simple = "Simple Burger"
ppBurger Double = "Double Burger"
ppBurger Triple = "Triple Burger"
ppBurger (Layer i t b) = ppBurger b <> ", " <> pack (show i) <> " " <> pack (show t)


ppBurgerWithPrice :: Prices -> Burger -> Text
ppBurgerWithPrice prices burger = ppBurger burger <> ". Price: $" <> pack (show (getPrice prices burger))


burgerMenu :: [Burger]
burgerMenu = [Simple, Double, Triple]


burgerEmoji :: Burger -> Text
burgerEmoji _ = " 🍔 "


-- burgerPrice :: [(Burger, Double)]
-- burgerPrice = [(Simple, 5.0), (Double, 7.0), (Triple, 9.0)]


toppingMenu :: [Topping]
toppingMenu = [Tomato, Cheese, Egg, Onion , Bacon, Lettuce, Pickle, Mushroom]


-- | no matching topping found will be represented with an empty string
toppingEmoji :: Topping -> Text
toppingEmoji Tomato = " 🍅 "
toppingEmoji Cheese = " 🧀 "
toppingEmoji Egg = " 🥚 "
toppingEmoji Onion = " 🧅 "
toppingEmoji Bacon = " 🥓 "
toppingEmoji Lettuce = " 🥬 "
toppingEmoji Pickle = " 🥒 "
toppingEmoji Mushroom = " 🍄 "
toppingEmoji Mayo = "" 
toppingEmoji Ketchup = "" 
toppingEmoji Mustard = "" 


-- toppingPrice :: [(Topping, Double)]
-- toppingPrice = [(Tomato, 1.0), (Cheese, 2.0), (Egg, 1.0), (Onion, 1.0), (Bacon, 3.0),  (Lettuce, 1.5), (Pickle, 1.5), (Mushroom, 3.0),(Mayo, 0.5), (Ketchup, 0.5), (Mustard, 0.5)]


sauceMenu :: [Topping]
sauceMenu = [Mayo, Ketchup, Mustard]


foldBurger :: (Int -> Topping -> b -> b) -> b -> b -> b -> Burger -> b
foldBurger l s d t (Layer i topping burger) = l i topping (foldBurger l s d t burger)
foldBurger l s d t Simple = s
foldBurger l s d t Double = d
foldBurger l s d t Triple = t


recBurger :: (Int -> Topping -> Burger -> b -> b) -> b -> b -> b -> Burger -> b
recBurger l s d t (Layer i topping burger) = l i topping burger (recBurger l s d t burger)
recBurger l s d t Simple = s
recBurger l s d t Double = d
recBurger l s d t Triple = t


burgerWith :: Burger -> Int -> Topping -> Burger
burgerWith burger amount topping = Layer amount topping burger


contains :: Burger -> Topping -> Bool
contains burger topping = foldBurger is False False False burger where
    is _ t b = b || t == topping


add :: Burger -> Topping -> Int -> Burger    
add burger topping amount = recBurger increment (append Simple) (append Double) (append Triple) burger where
    increment i t b h = if t == topping then Layer (i + amount) t b else Layer i t h
    append b = Layer amount topping b


getToppingPrice :: Prices -> Topping -> Double
getToppingPrice prices target = foldr (\(topping, price) r -> if topping == target then price else r) 0.0 (toppingsPrices prices)


getBurgerPrice :: Prices -> Burger -> Double
getBurgerPrice prices target = foldr (\(burger, price) r -> if burger == target then price else r) 0.0 (burgersPrices prices)


getPrice :: Prices -> Burger -> Double
getPrice prices (Layer i t b) = fromIntegral i * getToppingPrice prices t + getPrice prices b
getPrice prices size = getBurgerPrice prices size


orderPrice :: Model -> Double
orderPrice model = sum . map (getPrice (prices model)) . burgers $ model
