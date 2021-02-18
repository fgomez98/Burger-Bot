{-# LANGUAGE OverloadedStrings #-}

module Model.Db where

import           Data.Text
import           Data.Maybe
import           Data.List
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow ( ToRow(toRow) )
import           Control.Monad (void)
import           Configuration.Dotenv
import           System.Environment                  (getEnv)
import           Model.Burger
import           Model.BotModel as Model
import           Lib


-- | DAO's 
-- | ----------------------------------


data BurgerDao = BurgerDao {
    burger  :: Text,
    burgerCost    :: Double
} deriving (Show)

instance FromRow BurgerDao where
  fromRow = BurgerDao <$> field <*> field

instance ToRow BurgerDao where
  toRow d = [toField (burger d), toField (burgerCost d)]

data ToppingDao = ToppingDao {
    topping         :: Text,
    toppingCost     :: Double
} deriving (Show)

instance FromRow ToppingDao where
  fromRow = ToppingDao <$> field <*> field

instance ToRow ToppingDao where
  toRow d = [toField (topping d), toField (toppingCost d)]

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
    orderId                 :: Int,
    clientId_order          :: Int,
    orderClientFirstName    :: Text,
    orderClientLastName     :: Text,
    date                    :: UTCTime,
    completed               :: Bool,
    cost                    :: Double
} deriving (Show)

instance FromRow OrderDao where
  fromRow = OrderDao <$> field <*> field <*> field <*> field <*> field <*> field <*> field

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


-- | QUERIES
-- | ---------------------------------- 

-- | registers a burger, returns productId
insertBurger :: Burger -> IO Int
insertBurger (Layer amount topping burger) = do
  c <- connection
  productId <- insertBurger burger
  result <- (query c "INSERT INTO products_toppings (productid, topping, amount) VALUES (?, ?, ?) RETURNING productid"
      $ ProductToppingDao { productId_productTopping  = productId,
                            topping_productTopping    = pack (show topping),
                            amount                    = amount } 
      :: IO [Only Int])
  return (fromOnly (Prelude.head result))
insertBurger burger = do 
  c <- connection
  result <- (query c "INSERT INTO products (burger) VALUES (?) RETURNING productid" 
            $ Only (pack (show burger)) 
            :: IO [Only Int])
  return (fromOnly (Prelude.head result))            


-- | registers a client if not present, returns clientId
insertClient :: Client -> IO Int 
insertClient client = do 
  c <- connection
  _ <- (query c "INSERT INTO clients (clientId, firstName, lastName) VALUES (?, ?, ?) ON CONFLICT DO NOTHING RETURNING clientId" 
            $ ClientDao { Model.Db.clientId   = fromIntegral (Model.clientId client), 
                          Model.Db.firstName  = clientFirstName client,
                          Model.Db.lastName   = clientLastName client }
            :: IO [Only Int])
  return (fromIntegral (Model.clientId client))


-- | registers an order from a client, returns orderId
insertOrder :: Client -> [Double] -> [Burger] -> IO Int
insertOrder client burgersPrices burgers = do
  clientId <- insertClient client
  productsIds <- mapM insertBurger burgers
  let productsPrice = Prelude.zip productsIds burgersPrices
  c      <- connection
  result <- (query c "INSERT INTO orders (clientId, date) VALUES (?, ?) RETURNING orderid" 
            $ OrderDao {  clientId_order  = clientId,
                          Model.Db.date   = Model.date client }      
            :: IO [Only Int])
  let orderId = fromOnly (Prelude.head result)
  mapM_ ( \(productId, price) -> 
            execute c "INSERT INTO orders_products (orderid, productid, cost) VALUES (?, ?, ?)"
            $ OrderProductDao { orderId_orderProduct    = orderId,
                                productId_orderProduct  = productId,
                                cost_orderProduct       = price } )
        productsPrice
  return orderId


-- | marks an order as completed
completeOrder :: Int -> IO ()
completeOrder orderId = do 
  c <- connection
  _ <- execute c "UPDATE orders SET completed = true WHERE orderId = ?" $ Only (orderId)
  return ()


-- | I just wanted to change function name 
foldQuery :: (FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> IO a) -> IO a
foldQuery = fold


data Filters = Filters {
    filterClientId  :: Maybe Int,
    filterFromDate  :: Maybe UTCTime,
    filterToDate    :: Maybe UTCTime,
    filterFromCost  :: Maybe Double,
    filterToCost    :: Maybe Double,
    filterCompleted :: Maybe Bool
} deriving (Show)


defaultFilters :: Filters
defaultFilters = Filters Nothing Nothing Nothing Nothing Nothing Nothing


-- | get all orders that match arguments
selectOrders :: Filters -> IO [OrderDao]
selectOrders maybeFilters = do 
  let
    whereList = Data.Maybe.catMaybes -- The catMaybes function takes a list of Maybes and returns a list of all the Just values.
               [ const " clientId =  ? "  <$> filterClientId maybeFilters
               , const " date >= ? "      <$> filterFromDate maybeFilters
               , const " date <= ? "      <$> filterToDate maybeFilters
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
               [ const " sum(cost) >= ? " <$> filterFromCost maybeFilters
               , const " sum(cost) <= ? " <$> filterToCost maybeFilters ]      
    whereQuery = if Data.List.null whereList
           then mempty
           else "WHERE " <> mconcat ( Data.List.intersperse " AND " whereList)
    havingQuery = if Data.List.null havingList
           then mempty
           else "HAVING " <> mconcat ( Data.List.intersperse " AND " havingList)   
    q = "SELECT o.orderid as orderid, o.clientid as clientid, firstname, lastname, date, completed, sum(cost) as cost FROM orders AS o JOIN orders_products AS op on o.orderid = op.orderid JOIN clients AS c ON o.clientid = c.clientid "
        <> whereQuery 
        <> "GROUP BY o.orderid, c.clientid, date, completed, firstname, lastname "
        <> havingQuery
        <> "ORDER BY date ASC"
  c <- connection  
  query c q paramList    


-- | get all orders that match arguments
selectOrder :: Int -> IO OrderDao
selectOrder orderId = do 
  let  q =  "SELECT o.orderid as orderid, o.clientid as clientid, firstname, lastname, date, completed, sum(cost) as cost \
            \FROM orders AS o \
            \JOIN orders_products AS op on o.orderid = op.orderid \
            \JOIN clients AS c ON o.clientid = c.clientid \
            \WHERE o.orderid = ? \
            \GROUP BY o.orderid, c.clientid, date, completed, firstname, lastname "
  c       <- connection  
  result  <- query c q $ Only orderId
  return (Prelude.head result)


-- | get all orders that match arguments
selectClient :: Int -> IO [ClientDao]
selectClient clientId = do 
  c <- connection  
  query c "SELECT * FROM clients WHERE clientId = ?" $ Only clientId


-- | get all products from an order
selectProducts :: Int -> IO [Burger]
selectProducts orderId = do
  c <- connection
  foldQuery c "SELECT productid FROM orders_products WHERE orderid = ?" (Only orderId) [] foldProductsConsumer
  

foldProductsConsumer :: [Burger] -> Only Int -> IO [Burger]
foldProductsConsumer acc row = do
  let productId = fromOnly row
  c       <- connection
  pDaos   <- (query c "SELECT * FROM products WHERE productid = ?" $ Only productId :: IO [ProductDao])
  ptDaos  <- (query c "SELECT * FROM products_toppings WHERE productid = ?" $ Only productId :: IO [ProductToppingDao])
  return (burgerFrom (Prelude.head pDaos) ptDaos : acc)


burgerFrom :: ProductDao -> [ProductToppingDao] -> Burger
burgerFrom pDao = Prelude.foldr 
  (\ptDao burger -> Layer (amount ptDao) (read ((unpack . topping_productTopping) ptDao) :: Topping) burger) (read ((unpack . burger_product) pDao) :: Burger)


selectBurgersPrices :: IO [(Burger, Double)]
selectBurgersPrices = do
  c     <- connection
  bDaos <- query_ c "select * from burgers"
  return (Prelude.map (\bDao -> toTouple (read ((unpack . burger) bDao) :: Burger) (burgerCost bDao)) bDaos)


selectToppingsPrices :: IO [(Topping, Double)]
selectToppingsPrices = do
  c     <- connection
  tDaos <- query_ c "select * from toppings"
  return (Prelude.map (\tDao -> toTouple (read ((unpack . topping) tDao) :: Topping) (toppingCost tDao)) tDaos)


-- | Connection to postgresql database
connection :: IO Connection
connection = do 
  void $ loadFile defaultConfig
  dbName    <- getEnv "DB_NAME"
  dbUser    <- getEnv "DB_USER"
  dbPasswd  <- getEnv "DB_PASSWD"
  dbHost    <- getEnv "DB_HOST"
  -- dbPort    <- getEnv "DB-PORT"
  connect
      defaultConnectInfo
      { connectHost     = dbHost
      , connectDatabase = dbName
      , connectUser     = dbUser
      -- , connectPort     = dbPort
      ,connectPassword  = dbPasswd
      }
