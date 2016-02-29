module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


fib :: Integer -> Integer
fib n = fib' n 0 1

fib' :: Integer -> Integer -> Integer -> Integer
fib' n a b
  | n == 0 = a
  | otherwise = fib' (n-1) b (a+b)

-- main = print $ fib 10
