{-# LANGUAGE OverloadedStrings #-}
module Model.Model where
    
import GHC.Int (Int32)

import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import Lib

data User = User { userId :: Int32, firstName :: Text, lastName :: Text } deriving (Show)
-- data Burger = Burger Paty Topping

-- data Paty = Simple | Double | Triple deriving (Show, Eq)
-- data Topping = Tomato | Cheese | Egg | Onion | Bacon |Lettuce | Pickle | Mushroom deriving (Show, Eq)
-- data Sauce = Mayo | Ketchup | Mustard deriving (Show, Eq)
-- data Bread = White | Herbs | Grains | GlutenFree  deriving (Show, Eq)


data Model = Model
  { burgers :: [Burger],
    currentBurger :: Maybe Burger
  } deriving (Show)


data Client = Client
  { clientId :: Int32,
    clientFirstName :: Text,
    clientLastName :: Text,
    date :: UTCTime
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


toTouple :: a -> b -> (a,b)
toTouple a b = (a, b)


ppOrder :: Model -> Text
ppOrder model = case foldMap (withNewLine . (\(b, i) -> pack (show i) <> ". " <> ppBurgerWithPrice b)) (zipWith toTouple (burgers model) [1..]) of
    ""    -> "Your order is empty. Type /menu to add a burger to your order"
    items -> "Your order:\n" <> items <> "\nTotal: $" <> pack (show (orderPrice model))


data Burger =  Layer Int Topping Burger | Simple | Double | Triple | Empty deriving (Show, Eq, Read)
data Topping = Tomato | Cheese | Egg | Onion | Bacon |Lettuce | Pickle | Mushroom | Mayo | Ketchup | Mustard  deriving (Show, Eq, Read)
-- this is a shorter version:
-- data Topping = Tomato | Cheese | Egg | Onion | Bacon | Mayo | Ketchup | Mustard deriving (Show, Eq, Read)


ppBurger :: Burger -> Text
ppBurger Simple = "Simple Burger"
ppBurger Double = "Double Burger"
ppBurger Triple = "Triple Burger"
ppBurger (Layer i t b) = ppBurger b <> ", " <> pack (show i) <> " " <> pack (show t)


ppBurgerWithPrice :: Burger -> Text
ppBurgerWithPrice burger = ppBurger burger <> ". Price: $" <> pack (show (getPrice burger))

-- burgerMenu :: [(Text, Burger)]
-- burgerMenu = [("Simple", Simple), ("Double", Double), ("Triple", Triple)]
burgerMenu :: [Burger]
burgerMenu = [Simple, Double, Triple]


burgerEmoji :: Burger -> Text
burgerEmoji _ = " 🍔 "

burgerPrice :: [(Burger, Double)]
burgerPrice = [(Simple, 5.0), (Double, 7.0), (Triple, 9.0)]


-- toppingMenu :: [(Text, Topping)]
-- toppingMenu = [("Tomato", Tomato), ("Cheese", Cheese), ("Egg", Egg), ("Onion", Onion), ("Bacon", Bacon)]

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


toppingPrice :: [(Topping, Double)]
toppingPrice = [(Tomato, 1.0), (Cheese, 2.0), (Egg, 1.0), (Onion, 1.0), (Bacon, 3.0),  (Lettuce, 1.5), (Pickle, 1.5), (Mushroom, 3.0),(Mayo, 0.5), (Ketchup, 0.5), (Mustard, 0.5)]


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


getToppingPrice :: Topping -> Double
getToppingPrice target = foldr (\(topping, price) r -> if topping == target then price else r) 0.0 toppingPrice


getBurgerPrice :: Burger -> Double
getBurgerPrice target = foldr (\(burger, price) r -> if burger == target then price else r) 0.0 burgerPrice


getPrice :: Burger -> Double
getPrice (Layer i t b) = fromIntegral i * getToppingPrice t + getPrice b
getPrice size = getBurgerPrice size


orderPrice :: Model -> Double
orderPrice = sum . map getPrice . burgers

-- remove :: Burger -> Topping -> Burger
-- remove burger topping = foldBurger take False False False burger where
--     take (Layer i t burger) = if t == topping then burger else Layer i t burger

-- -- Monads for try catch
-- remove :: Burger -> Int -> Topping -> Burger
-- remove burger amount topping = foldBurger take False False False burger where
--     take (Layer i t burger) = if t == topping then burger else Layer (i - amount) t burger    

-- La idea es que junte las cantidades de todo, otra forma de solucionarlo es ver si ya esta el ingrediente y no darle la opcion al cliente
-- normalize :: Burger -> Burger
