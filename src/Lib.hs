module Lib where

import Data.Char
import Data.Word

showByte :: Word8 -> String
showByte x = [chr (read $ show x :: Int)]