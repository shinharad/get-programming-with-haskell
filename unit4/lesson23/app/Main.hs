module Main where

import qualified Data.Text.IO as TIO
import           Lib

main = do
  TIO.putStrLn (highlight dharma bgText)
