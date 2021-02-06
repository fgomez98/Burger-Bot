{-# LANGUAGE OverloadedStrings #-}

module Model.Db where

import Data.Text
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Int(Int64)
import           Control.Monad.Trans              (liftIO)
import           Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow ( ToRow(toRow) )
import Model.Model as Model
import Lib


data BurgerDao = BurgerDao {
    burger :: Text
} deriving (Show)

instance FromRow BurgerDao where
  fromRow = BurgerDao <$> field

instance ToRow BurgerDao where
  toRow d = [toField (burger d)]

data ToppingDao = ToppingDao {
    topping :: Text
} deriving (Show)

instance FromRow ToppingDao where
  fromRow = ToppingDao <$> field

instance ToRow ToppingDao where
  toRow d = [toField (topping d)]

data ClientDao = ClientDao {
    clientId    :: Int,
    -- telegramId  :: Int,
    firstName   :: Text,
    lastName    :: Text
} deriving (Show)

instance FromRow ClientDao where
  fromRow = ClientDao <$> field <*> field <*> field

instance ToRow ClientDao where
  toRow d = [toField (Model.Db.clientId d), toField (Model.Db.firstName d), toField (Model.Db.lastName d)]

data OrderDao = OrderDao {
    orderId           :: Int,
    clientId_order    :: Int,
    date              :: UTCTime,
    completed         :: Bool,
    cost              :: Double
} deriving (Show)

instance FromRow OrderDao where
  fromRow = OrderDao <$> field <*> field <*> field <*> field <*> field

instance ToRow OrderDao where
  toRow d = [toField (clientId_order d), toField (Model.Db.date d)]

data ProductDao = ProductDao {
    productId         :: Int,
    burger_product    :: Text
} deriving (Show)

instance FromRow ProductDao where
  fromRow = ProductDao <$> field <*> field

instance ToRow ProductDao where
  toRow d = [toField (productId d), toField (burger_product d)]

data ProductToppingDao = ProductToppingDao {
    productId_productTopping  :: Int,
    topping_productTopping    :: Text,
    amount                    :: Int
} deriving (Show)

instance FromRow ProductToppingDao where
  fromRow = ProductToppingDao <$> field <*> field <*> field

instance ToRow ProductToppingDao where
  toRow d = [toField (productId_productTopping d), toField (topping_productTopping d), toField (amount d)]

data OrderProductDao = OrderProductDao {
    orderId_orderProduct        :: Int,
    productId_orderProduct      :: Int,
    cost_orderProduct           :: Double
} deriving (Show)

instance FromRow OrderProductDao where
  fromRow = OrderProductDao <$> field <*> field <*> field

instance ToRow OrderProductDao where
  toRow d = [toField (orderId_orderProduct d), toField (productId_orderProduct d), toField (cost_orderProduct d)]


-- burger2BurgerDao :: Burger -> BurgerDao
-- burger2BurgerDao = foldBurger (const . const id) (BurgerDao "Simple") (BurgerDao "Double") (BurgerDao "Triple")


-- | registers a burger, returns productId
insertBurger :: Burger -> IO Int
insertBurger (Layer amount topping burger) = do
  c <- connection
  productId <- insertBurger burger
  result <- (query c "INSERT INTO products_toppings (productid, topping, amount) VALUES (?, ?, ?) RETURNING productid"
            $ 
            ProductToppingDao { productId_productTopping = productId,
                                topping_productTopping = pack (show topping),
                                amount = amount }
            :: IO [Only Int])
  pure (fromOnly (Prelude.head result))
insertBurger burger = do 
  c <- connection
  result <- (query c "INSERT INTO products (burger) VALUES (?) RETURNING productid" 
            $
            Only (pack (show burger))
            :: IO [Only Int])
  pure (fromOnly (Prelude.head result))


-- | registers a client if not present, returns clientId
insertClient :: Client -> IO Int 
insertClient client = do 
  c <- connection
  _ <- (query c "INSERT INTO clients (clientId, firstName, lastName) VALUES (?, ?, ?) ON CONFLICT DO NOTHING RETURNING clientId" 
            $
            ClientDao { Model.Db.clientId   = fromIntegral (Model.clientId client), 
                        Model.Db.firstName  = clientFirstName client,
                        Model.Db.lastName   = clientLastName client }
            :: IO [Only Int])
  pure (fromIntegral (Model.clientId client))


-- | registers an order from a client, returns orderId
insertOrder :: Client -> [Burger] -> IO Int
insertOrder client burgers = do
  clientId <- insertClient client
  productsIds <- mapM insertBurger burgers
  let productsPrice = Prelude.zip productsIds (Prelude.map getPrice burgers)
  c <- connection
  result <- (query c "INSERT INTO orders (clientId, date) VALUES (?, ?) RETURNING orderid" 
            $
            OrderDao {  clientId_order  = clientId,
                        Model.Db.date   = Model.date client }      
            :: IO [Only Int])
  let orderId = fromOnly (Prelude.head result)
  mapM_ ( \(productId, price) -> 
            execute c "INSERT INTO orders_products (orderid, productid, cost) VALUES (?, ?, ?)"
            $
            OrderProductDao { orderId_orderProduct    = orderId,
                              productId_orderProduct  = productId,
                              cost_orderProduct       = price } )
        productsPrice
  pure orderId


-- | marks an order as completed
completeOrder :: Int -> IO ()
completeOrder orderId = do 
  c <- connection
  _ <- execute c "UPDATE orders SET completed = true WHERE orderId = ?" $ Only (orderId)
  return ()


-- | I just wanted to change function name 
foldQuery = fold

data Filters = Filters {
    filterClientId  :: Maybe Int,
    filterFromDate  :: Maybe UTCTime,
    filterToDate    :: Maybe UTCTime,
    filterFromCost  :: Maybe Double,
    filterToCost    :: Maybe Double,
    filterCompleted :: Maybe Bool
}

defaultFilters :: Filters
defaultFilters = Filters Nothing Nothing Nothing Nothing Nothing Nothing

-- | get all orders that match arguments
selectOrders :: Filters -> IO [OrderDao]
selectOrders maybeFilters = do 
  let
    whereList = Data.Maybe.catMaybes -- The catMaybes function takes a list of Maybes and returns a list of all the Just values.
               [ const " clientId =  ? "  <$> filterClientId maybeFilters
               , const " date >= ? "      <$> filterFromDate maybeFilters
               , const " date < ? "       <$> filterToDate maybeFilters
               , const " completed = ? "  <$> filterCompleted maybeFilters ]
    paramList = Data.Maybe.catMaybes
                [ -- where params 
                  toField <$> filterClientId maybeFilters
                , toField <$> filterFromDate maybeFilters
                , toField <$> filterToDate maybeFilters
                , toField <$> filterCompleted maybeFilters 
                  -- having params
                , toField <$> filterFromCost maybeFilters
                , toField <$> filterToCost maybeFilters ]
    havingList = Data.Maybe.catMaybes
               [ const " sum(cost) >= ? "   <$> filterFromCost maybeFilters
               , const " sum(cost) < ? "    <$> filterToCost maybeFilters ]      
    whereQuery = if Data.List.null whereList
           then mempty
           else "WHERE " <> mconcat ( Data.List.intersperse " AND " whereList)
    havingQuery = if Data.List.null havingList
           then mempty
           else "HAVING " <> mconcat ( Data.List.intersperse " AND " havingList)   
    q = "SELECT o.orderid as orderid, clientid, date, completed, sum(cost) as cost FROM orders AS o JOIN orders_products AS op on o.orderid = op.orderid  "
        <> whereQuery 
        <> "GROUP BY o.orderid, clientid, date, completed "
        <> havingQuery
  c <- connection  
  query c q paramList    


-- | get all products from an order
selectProducts :: Int -> IO [Burger]
selectProducts orderId = do
  c <- connection
  foldQuery c "SELECT productid FROM orders_products WHERE orderid = ?" (Only orderId) [] foldProductsConsumer
  

foldProductsConsumer :: [Burger] -> Only Int -> IO [Burger]
foldProductsConsumer acc row = do
  let productId = fromOnly row
  c <- connection
  pDaos <- (query c "SELECT * FROM products WHERE productid = ?" $ Only productId :: IO [ProductDao])
  ptDaos <- (query c "SELECT * FROM products_toppings WHERE productid = ?" $ Only productId :: IO [ProductToppingDao])
  return (burgerFrom (Prelude.head pDaos) ptDaos : acc)

burgerFrom :: ProductDao -> [ProductToppingDao] -> Burger
burgerFrom pDao = Prelude.foldr 
  (\ptDao burger -> Layer (amount ptDao) (read ((unpack . topping_productTopping) ptDao) :: Topping) burger) (read ((unpack . burger_product) pDao) :: Burger)

selectBurgerPrice :: Burger -> IO Double
selectBurgerPrice burger = do
  c <- connection
  price <- (query c "select cost from burgers where burger = ?" $ Only (show burger) :: IO [Only Double])
  return (fromOnly (Prelude.head price))

selectToppingPrice :: Topping -> IO Double
selectToppingPrice topping = do
  c <- connection
  price <- (query c "select cost from toppings where topping = ?" $ Only (show topping) :: IO [Only Double])
  return (fromOnly (Prelude.head price))  

-- | Connection to postgresql database
connection :: IO Connection
connection = connect
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "telegram_bot_db"
      , connectUser = "fermingomez"
      }
