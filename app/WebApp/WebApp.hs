{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Control.Applicative ((<$>), (<*>))
import            Data.Maybe (isJust)
import            Web.Spock
import            Web.Spock.Config
import            Data.Time.Clock
import            Control.Monad
import            Control.Monad.Trans
import            Data.Text (Text, unpack)
import            Data.Text.Lazy (toStrict)
import qualified  Text.Blaze.Html.Renderer.Text as R
import            Data.Text.Time (parseUTCTimeOrError)

import            WebApp.Views
import            WebApp.Conf
import            Model.Model
import            Model.Db


main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock port (spock spockCfg app)


-- SpockM database session state ()
app :: SpockM () () () ()
app = do 
  get root $ do
    pendingOrders <- liftIO $ selectOrders $ Filters {
      filterClientId  = Nothing,
      filterFromDate  = Nothing,
      filterToDate    = Nothing,
      filterFromCost  = Nothing,
      filterToCost    = Nothing,
      filterCompleted = Just False
    }
    html . toStrict $ R.renderHtml $ pendingOrdersHTML pendingOrders
  getpost "search" $ do
    ps <- params 
    pendingOrders <- liftIO $ selectOrders $ Filters {
      filterClientId  = asInt <$> getParam "clientId" ps,
      filterFromDate  = asDate <$> getParam "fromDate" ps,
      filterToDate    = asDate <$> getParam "toDate" ps,
      filterFromCost  = asDouble <$> getParam "fromCost" ps,
      filterToCost    = asDouble <$> getParam "toCost" ps,
      filterCompleted = status2Bool $ getParam "status" ps
    }
    html . toStrict $ R.renderHtml $ allOrdersHTML pendingOrders  
  get ("order" <//> var) $ \orderId -> do
    orderDao <- liftIO $ selectOrder orderId
    burgers <- liftIO $ selectProducts orderId
    burgersPrices <- liftIO selectBurgersPrices
    toppingsPrices <- liftIO selectToppingsPrices
    let route = renderRoute ("done" <//> var) orderId
    html . toStrict $ R.renderHtml $ orderProductsHTML orderDao burgers (Prices toppingsPrices burgersPrices)
  get ("done" <//> var) $ \orderId -> do
    liftIO $ completeOrder orderId
    redirect "/"  


getParam :: Text -> [(Text, Text)] -> Maybe Text 
getParam target = foldr (\(param, value) ret -> if h $ compare target param then ifPresent value else ret) Nothing where
  h EQ  = True
  h _   = False
  ifPresent ""  = Nothing
  ifPresent e   = Just e


asInt :: Text -> Int 
asInt e = (read . unpack) e :: Int


asDate :: Text -> UTCTime 
asDate e = let (Right date) = parseUTCTimeOrError e in date


asDouble :: Text -> Double 
asDouble e = (read . unpack) e :: Double


status2Bool :: Maybe Text -> Maybe Bool 
status2Bool status = do 
  x <- status
  case x of
    "All"           -> fail ""
    "Completed"     -> return True
    "Not Completed" -> return False
    _               -> fail ""        


landingHTML :: Text
landingHTML =
  "<html>\
    \<body>\
      \<p>Hello world! Please enter your username!\
      \<form action=\"/hello\" method=\"post\">\
        \Username: <input type=\"text\" name=\"username\"><br>\
        \<input type=\"submit\"><br>\
      \</form>\
    \</body>\
  \</html>"
