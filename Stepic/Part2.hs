module Part2 where

import Data.Function

sumFstFst = (+) `on` helper
  where
    helper pp = fst $ fst pp


sumFstFst' = (+) `on` (\pp -> fst $ fst pp)

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on op f x y = f x `op` f y
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares = (\x y z -> x+y+z) `on3` (^2)
