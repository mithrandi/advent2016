module Main where

import Data.Foldable

data Tile = Safe | Trap

type Row = [Tile]

showTile :: Tile -> Char
showTile Safe = '.'
showTile Trap = '^'

instance Show Tile where
  show t = [showTile t]
  showList ts = (map showTile ts ++)

isSafe :: Tile -> Bool
isSafe Safe = True
isSafe Trap = False

parseRow :: String -> Row
parseRow = map parseTile

parseTile :: Char -> Tile
parseTile '.' = Safe
parseTile '^' = Trap
parseTile _ = error "invalid tile"

nextRow :: Row -> Row
nextRow row = nextTiles (Safe:row)
  where nextTiles (l:ts@(_:r:_)) = nextTile l r:nextTiles ts
        nextTiles [l,_] = [nextTile l Safe]
        nextTiles _ = []
        nextTile Trap Safe = Trap
        nextTile Safe Trap = Trap
        nextTile _    _    = Safe

countSafe :: [Row] -> Int
countSafe = sum . map (length . filter isSafe)

main :: IO ()
main = do
  putStrLn "Example 1:"
  traverse_ print (take 3 . iterate nextRow . parseRow $ "..^^.")
  putStrLn "Example 2:"
  let exampleRoom = take 10 . iterate nextRow . parseRow $ ".^^.^.^^^^"
  traverse_ print exampleRoom
  print $ countSafe exampleRoom
  input <- head . lines <$> readFile "Day18.txt"
  putStrLn "Part 1:"
  let infiniteRoom = iterate nextRow . parseRow $ input
  print . countSafe . take 40 $ infiniteRoom
  putStrLn "Part 2:"
  print . countSafe . take 400000 $ infiniteRoom
