{-# LANGUAGE OverloadedStrings #-}

module Model.Db where

import Data.Text
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
    completed         :: Bool
} deriving (Show)

instance FromRow OrderDao where
  fromRow = OrderDao <$> field <*> field <*> field <*> field

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


burger2BurgerDao :: Burger -> BurgerDao
burger2BurgerDao = foldBurger (const . const id) (BurgerDao "Simple") (BurgerDao "Double") (BurgerDao "Triple")


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
  c <- connection
  result <- (query c "INSERT INTO orders (clientId, date) VALUES (?, ?) RETURNING orderid" 
            $
            OrderDao {  clientId_order  = clientId,
                        Model.Db.date            = Model.date client }      
            :: IO [Only Int])
  let orderId = fromOnly (Prelude.head result)
  mapM_ ( \productId -> 
            execute c "INSERT INTO orders_products (orderid, productid, cost) VALUES (?, ?, ?)"
            $
            OrderProductDao { orderId_orderProduct = orderId,
                              productId_orderProduct = productId,
                              cost_orderProduct = 2.0 } )
        productsIds
  pure orderId


-- | marks an order as completed
completeOrder :: Int -> IO ()
completeOrder orderId = do 
  c <- connection
  _ <- execute c "UPDATE orders SET completed = true WHERE orderId = ?" $ Only (orderId)
  return ()

date2SQLDateRange :: (UTCTime, UTCTime) -> Text
date2SQLDateRange (from, to) = " BETWEEN " <> pack (show from) <> " AND " <> pack (show to) <> " "

bool2SQLBool :: Bool -> Text
bool2SQLBool True = "true" 
bool2SQLBool False = "flase"

int2SQLInt :: Int -> Text
int2SQLInt = showText 

-- -- | get all orders according to maybe filters
-- selectOrders :: Maybe Int -> Maybe (UTCTime, UTCTime) -> Maybe Bool -> IO [OrderDao]
-- selectOrders maybeClientId maybeDate maybeCompleted = do 
--   let queryFilter = clientFilter <> dateFilter <> completedFilter
--   let queryStatment = "SELECT * FROM orders" <> (if queryFilter == "" then "WHERE" else "") <> queryFilter
--   c <- connection
--   query_ c queryStatment <> "" where
--     clientFilter = (tryGetValue (maybeClientId >>= (\e -> Just $ " clientId = " <> int2SQLInt e)) "")
--     dateFilter = (tryGetValue (maybeDate >>= (\e -> Just $ " AND date " <> date2SQLDateRange e)) "")
--     completedFilter = (tryGetValue (maybeCompleted >>= (\e -> Just $ " AND completed == " <> bool2SQLBool e)) "")



-- | get all products from an order
-- selectProducts :: Int -> IO [OrderProductDao]

-- selectProduct :: Int -> IO [Burger]

-- | Connection to postgresql database
connection :: IO Connection
connection = connect
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "telegram_bot_db"
      , connectUser = "fermingomez"
      }
