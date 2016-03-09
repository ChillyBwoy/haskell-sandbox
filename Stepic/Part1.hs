module Demo where

import Data.Char

discount :: Double -> Double -> Double -> Double
discount limit proc sum =
  if sum >= limit then sum * (100 - proc) / 100 else sum


standardDiscount :: Double -> Double
standardDiscount = discount 1000 5


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y =
  if isDigit x && isDigit y
    then
      let x' = digitToInt x
          y' = digitToInt y
      in x' * 10 + y'
    else 100

-- AB = √(xb - xa)2 + (yb - ya)2
-- sqrt(11^2-10^2+23^2-10^2)
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 =
  let (x1, y1) = p1
      (x2, y2) = p2
  in
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


factorial :: Integer -> Integer
factorial n  = if n == 0 then 1 else n * factorial (n - 1)

factorial1 :: Integer -> Integer
factorial1 0 = 1
factorial1 n = n * factorial1 (n - 1)

factorial2 :: Integer -> Integer
factorial2 0 = 1
factorial2 n = if n < 0 then error "arg must be >= 0" else n * factorial2 (n - 1)

factorial3 :: Integer -> Integer
factorial3 0 = 1
factorial3 n | n < 0 = error "arg must be >= 0"
             | n > 0 = n * factorial3 (n - 1)

factorial4 :: Integer -> Integer
factorial4 n | n == 0    = 1
             | n > 0     = n * factorial4 (n - 1)
             | otherwise = error "arg must be >= 0"

factorial5 n | n >= 0    = helper 1 n
             | otherwise = error "arg must be >= 0"

factorial6 :: Integer -> Integer
factorial6 n
  | n >= 0 = let
      helper acc 0 = acc
      helper acc n = helper (acc * n) (n - 1)
    in helper 1 n
  | otherwise = error "arg must be >= 0"


factorial7 :: Integer -> Integer
factorial7 n | n >= 0    = f' n 1
             | otherwise = error "arg must be >= 0"
  where
    f' 0 acc = acc
    f' n acc = f' (n - 1) (acc * n)


helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)


doubleFact :: Integer -> Integer
doubleFact n | n < 0                    = error "arg must be >= 0"
             | n == 0                   = 1
             | n == 1 && n `mod` 2 == 1 = 1
             | otherwise                = n * doubleFact (n - 2)


fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0  = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0  = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci' :: Integer -> Integer
fibonacci' n = fib n (0, 1)
  where
    fib 0 (a, b) = a
    fib n (a, b) | n > 0 = fib (n - 1) (a + b, a)
                 | n < 0 = fib (n + 1) (a + b, a)

seqA :: Integer -> Integer
seqA n = let
  fn 0 = 1
  fn 1 = 2
  fn 2 = 3
  fn n' = fn (n' - 1) + fn (n' - 2) - 2 * fn (n' - 3)
  in fn n

seqA' :: Integer -> Integer
seqA' n = let
  fn 0 (_, _, c) = c
  fn 1 (_, b, _) = b
  fn 2 (a, _, _) = a
  fn n (a, b, c) = fn (n - 1) (a + b - 2 * c, a, b)
  in fn n (3, 2, 1)


roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
  (
    (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  ,
    (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  )


roots' :: Double -> Double -> Double -> (Double, Double)
roots' a b c =
  let d = sqrt (b ^ 2 - 4 * a * c) in
  ((-b - d) / (2 * a), (-b + d) / (2 * a))


rootsDiff a b c = let
  (x1, x2) = roots a b c
  in x2 - x1


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = fn (abs x) (0, 0)
  where
    fn 0 (sumX, countX) = (sumX, countX)
    fn x (sumX, countX) = let
      (x', r') = quotRem x 10
      in fn x' (sumX + r', countX + 1)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = undefined
