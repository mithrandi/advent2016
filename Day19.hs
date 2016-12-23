module Main where

import Data.List
import Data.Maybe

data Elf = Elf
           { position :: Int
           , presents :: Int
           }
 deriving (Show, Eq)

elves :: Int -> [Elf]
elves n = map (`Elf` 1) [1..n]

narrowWith :: (a -> a -> a) -> [a] -> [a]
narrowWith f = go
  where go (x:x':xs) = f x x':go xs
        go [x] = [x]
        go [] = []

play :: Int -> Elf
play n = fromJust . find ((== n) . presents) $ es'
  where es' = elves n ++ narrowWith steal es'
        steal (Elf n1 p1) (Elf _ p2) = Elf n1 (p1 + p2)

main :: IO ()
main = do
  print $ play 5
  print $ play 3014387
