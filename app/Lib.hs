module Lib where

import qualified Telegram.Bot.API                 as Telegram
import           Data.Text                        (Text, pack)

import GHC.Int (Int32)

data MayFail a = Raise Exception | Ok a deriving (Show)
data Exception = DivByZero | NotFound | NullPointer | Other String deriving (Show)
type ExHandler a = Exception -> a 

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Ok a) function _ = function a 
tryCatch (Raise exception) _ handler = handler exception

tryGetValue :: Maybe a -> a -> a
tryGetValue (Just value) _ = value
tryGetValue Nothing value = value

ifPresentOrElse :: Maybe a -> (a -> b) -> b -> b
ifPresentOrElse (Just value) f _ = f value
ifPresentOrElse Nothing f orElse =  orElse

getId :: Telegram.User -> Int32
getId user = let (Telegram.UserId val) = Telegram.userId user in val

takeAt :: Int -> [a] -> [a]
takeAt = flip (foldr aux (const [])) where 
    aux x h 0 = h (-1)
    aux x h n = x : h (n-1)

string2Text :: String -> Text    
string2Text = pack

showText :: Show a => a -> Text    
showText = pack . show