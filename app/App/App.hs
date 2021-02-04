module Main where
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Calculator"
             , windowResizable    := False
             , windowDefaultWidth  := 230
             , windowDefaultHeight := 250 ]
  widgetShowAll window
  mainGUI