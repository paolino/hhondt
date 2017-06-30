-- | Implementation of D'Hondt method 
-- <https://en.wikipedia.org/wiki/D%27Hondt_method>
module Hondt (assign) where


import Control.Arrow     ((&&&))
import Data.List    (sort, groupBy, group)
import Data.Ord      (comparing)
import Data.Function    (on)
import Data.Ratio      ((%))
  

-- two descending ordered lists mixer, keeping ties
mergedown :: Ord a
       => (b -> a) -> [b] -> [b] -> [b]
mergedown _ [] x  = x
mergedown _ x  [] = x
mergedown f xt@(x:xs) yt@(y:ys)
    | f x > f y  = x : mergedown f xs yt
    | f x < f y  = y : mergedown f xt ys
    | otherwise  = x : y : mergedown f xs ys

-- explosion
hondt :: [Int] -> [[(Int, Rational)]]
hondt = zipWith (\ n p -> zip (repeat n)
            $ map (fromIntegral p %) [1..]) [0..]


-- | D'hondt assignment method
assign     :: Int  -- ^ seats
    -> [Int] -- ^ votes per party
    -> [Int] -- ^ seats per party
assign s ps
    = map (sum . map snd)                   -- sum defaults and values
    . groupBy ((==) `on` fst)               -- group by index
    . sort                                  -- sort to bring keys together
    . (++ zipWith (\_ n -> (n,0)) ps [0..]) -- add 0 as defaults
    . map (head &&& length)                 -- count evenience of indexes
    . group                                 -- group
    . sort                                  -- sort
    . map fst                               -- keep the indexes
    . take s                                -- take s eveniences
    . foldr1  (mergedown snd)                  -- mixup by descending value
    . hondt                                 -- explode hondts
    $ ps

