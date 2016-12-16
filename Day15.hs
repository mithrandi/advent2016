module Main where

import Data.List

data Disc = Disc
  { initial :: Int
  , size :: Int
  }

positions :: [Disc] -> [[Int]]
positions = map positions'
  where positions' (Disc i s) = cycle ([i..s-1] ++ [0..i-1])

discs :: [Disc]
discs =
  [ Disc 1 17
  , Disc 0 7
  , Disc 2 19
  , Disc 0 5
  , Disc 0 3
  , Disc 5 13
  ]

discs2 :: [Disc]
discs2 = discs ++ [Disc 0 11]

example :: [Disc]
example =
  [ Disc 4 5
  , Disc 1 2
  ]

machine :: [[Int]] -> Bool
machine = machine' 1
  where machine' time' (p:ps) = p !! time' == 0 && machine' (time' + 1) ps
        machine' _ [] = True

main :: IO ()
main = do
  print $ machine (map (drop 5) (positions example))
  print . findIndex machine $ iterate (map tail) (positions example)
  print . findIndex machine $ iterate (map tail) (positions discs)
  print . findIndex machine $ iterate (map tail) (positions discs2)
