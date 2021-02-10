{-# LANGUAGE OverloadedStrings #-}

module App.Views where

import            Text.Blaze.Html5 as H
import            Text.Blaze.Html5.Attributes as A
import            Prelude hiding (head, id, div)
import            Text.Blaze.Html
import            Control.Monad
import            Data.Text (pack, Text)

import            Model.Db
import            Model.Model
import            App.Conf
import            Lib


navItemHTML :: Bool -> Text -> Text -> Html
navItemHTML True label ref = do
    H.li ! class_ "nav-item active" $ do
        H.a ! class_ "nav-link" ! A.href (textValue ref) $ toHtml label
        H.span !class_ "sr-only" $ "(current)"        
navItemHTML False label ref = do
    H.li ! class_ "nav-item" $ do
        H.a ! class_ "nav-link" ! A.href (textValue ref) $ toHtml label


headerHTML :: Bool -> Bool -> Html
headerHTML activePendingOrder activeAllOrders = do
    H.head $ do
        H.title "Burger Life"
        H.link ! A.rel "stylesheet" ! A.href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
        H.link ! A.href "https://fonts.googleapis.com/css?family=Lato:300,400,700,300italic,400italic,700italic" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.nav ! class_ "navbar navbar-expand-lg navbar-light bg-light" $ do
        H.a ! A.style "font-size: 28px" ! class_ "navbar-brand" $ "Burger Life"
        H.div ! class_ "collapse navbar-collapse" ! A.id "navbarSupportedContent" $ do  
            H.ul ! class_ "navbar-nav me-auto" $ do
                navItemHTML activePendingOrder "Pending Orders" (host <> ":" <> pack (show port))
                navItemHTML activeAllOrders "All Orders" (host <> ":" <> pack (show port) <> "/all/")


searchHTML :: Html
searchHTML = do
    H.div ! A.style "display: block; text-align: center; padding: 2em;" $ do
        H.form ! A.style " text-align: left" ! A.action (textValue (host <> ":" <> pack (show port) <> "/all/")) ! A.method "post" ! A.name "searchForm" $ do
            H.div ! class_ "form-group"  $ do
                H.div ! class_ "form-row" $ do
                    H.div ! class_ "col" $ do
                        H.label ! A.for "fromCost" $ "Min Cost"
                        H.input ! A.type_ "number" ! A.step "0.05" ! class_ "form-control" ! A.id "fromCost" ! A.name "fromCost"
                    H.div ! class_ "col" $ do
                        H.label ! for "toCost" $ "Max Cost"
                        H.input ! A.type_ "number" ! A.step "0.05" ! class_ "form-control" ! A.id "toCost" ! A.name "toCost"
            H.div ! class_ "form-group" $ do
                H.div ! class_ "form-row" $ do
                    H.div ! class_ "col" $ do
                        H.label ! A.for "fromDate" $ "From Date"
                        H.input ! A.type_ "date" ! class_ "form-control" ! A.id "fromDate" ! A.name "fromDate"
                    H.div ! class_ "col" $ do
                        H.label ! A.for "toDate" $ "To Date"
                        H.input ! A.type_ "date" ! class_ "form-control" ! A.id "toDate" ! A.name "toDate"
            H.div ! class_ "form-group" $ do
                H.label ! A.for  "clientId" $ "Client Id"
                H.input ! A.type_ "number" ! class_ "form-control" ! A.id "clientId" ! A.name "clientId"
            H.div ! class_ "form-group" $ do
                H.label ! A.for "controlSelect" $ "Order Status"
                H.select ! class_ "form-control" ! A.id "controlSelect" ! A.name "status"  $ do
                    H.option "All"
                    H.option "Completed"
                    H.option "Not Completed"
            H.button ! A.type_ "submit" ! class_ "btn btn-primary" $ "Submit"


allOrdersHTML :: [OrderDao] -> Html
allOrdersHTML  orders = H.html $ do
  headerHTML False True
  searchHTML
  ordersDaoTableHTML orders


pendingOrdersHTML :: [OrderDao] -> Html
pendingOrdersHTML orders = H.html $ do
  headerHTML True False
  ordersDaoTableHTML orders   


orderProductsHTML :: OrderDao -> [Burger] -> Prices -> Html
orderProductsHTML order burgers prices = H.html $ do
  headerHTML False False
  orderProductsCardHTML order burgers prices


ordersDaoTableRowHTML :: OrderDao -> Html
ordersDaoTableRowHTML order = do
    H.tr $ do
        H.th ! A.scope "row" $ toHtml (orderId  order)
        H.th $ toHtml (clientId_order order)
        H.td $ toHtml (orderClientFirstName order)
        H.td $ toHtml (orderClientLastName order)
        H.td $ toHtml (show $ Model.Db.date order)
        H.td $ toHtml (cost order)
        H.td $ if completed order
            then "Yes"
            else "No"
            -- H.form ! A.action (textValue ("done/" <> pack (show (orderId  order)))) ! A.method "get" $ do
                -- H.button ! class_ "btn btn-outline-success my-2 my-sm-0" ! A.type_ "submit" $ "Done"        
        H.td $ do
            H.form ! A.action (textValue (host <> ":" <> pack (show port) <> "/order/" <> pack (show (orderId  order)))) ! A.method "get" $ do
                H.button ! class_ "btn btn-outline-success my-2 my-sm-0" ! A.type_ "submit" $ "View"


ordersDaoTableHTML :: [OrderDao] -> Html
ordersDaoTableHTML os = do
    H.table ! class_ "table" $ do
        H.thead $ do
            H.tr $ do
                H.th "Order Id"
                H.th "Client Id"
                H.th "First Name"
                H.th "Last Name"
                H.th "Date"
                H.th "Cost"
                H.th "Completed"
                H.th ""-- This is for a view products button
        H.tbody $ forM_ os ordersDaoTableRowHTML


burgerListItemHTML :: Prices -> Burger ->  Html
burgerListItemHTML prices burger = do
    H.li ! class_ "list-group-item" $ do
        H.span ! class_"badge" $ do 
            toHtml (getPrice prices burger)
            " $ "
        toHtml (ppBurger burger)


completedActionHTML :: OrderDao -> Html 
completedActionHTML order = do 
    H.form ! A.style "display: block; text-align: center; padding: 1em;" ! A.action (textValue (host <> ":" <> pack (show port) <> "/done/" <> pack (show(orderId order)))) ! A.method "get" $ do
        H.button ! class_ "btn btn-outline-success my-2 my-sm-0" ! A.type_ "submit" $ "Mark as Completed"            


completedHTML :: Html 
completedHTML = do 
    H.div ! A.style "display: block; text-align: center; padding: 1em;" $ do
        H.label ! class_ "btn btn-success disabled no-click my-2 my-sm-0" $ "Order Completed"                    


orderProductsCardHTML :: OrderDao -> [Burger] -> Prices -> Html
orderProductsCardHTML order burgers prices = do
    H.div ! class_ "card" $ do
        H.div ! class_ "card-body" $ do
            H.h4 ! class_ "card-title" $ toHtml $ "Order " <> pack (show (orderId order))
            H.h5 ! class_"card-text" $ "Products: "
            case burgers of 
                [] ->   H.p ! class_"card-text" $ "Empty Order"
                bs ->   H.ul ! class_ "list-group" $ do
                    forM_ bs (burgerListItemHTML prices)
                    if completed order
                        then completedHTML
                        else completedActionHTML order
