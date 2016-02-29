module Main where

import Lib

simpleSum :: Int -> Int
simpleSum value = value + value


indicate :: String -> String
indicate address
  | address == "127.0.0.1" = "localhost"
  | null address           = "empty IP-address"
  | otherwise              = address


prepareLength :: Double -> Double
prepareLength line =
  line * coefficient - correction
  where coefficient = 0.4959
        correction = 0.0012



main :: IO ()
main = putStrLn (show (simpleSum 4))
