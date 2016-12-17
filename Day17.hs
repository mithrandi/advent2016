module Main where

import           Control.Applicative
import           Crypto.Hash
import           Data.ByteArray.Encoding
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.Maybe
import           Data.Range.Range

data Coord = Coord !Int !Int
  deriving (Read, Show, Ord, Eq)

data Move = MUp | MDown | MLeft | MRight
  deriving (Read, Show, Ord, Eq)

data Position = Position
                { key :: !C.ByteString
                , path :: ![Move]
                , coord :: !Coord
                }
  deriving (Read, Show, Ord, Eq)

{-# INLINE orEmpty #-}
orEmpty :: Alternative f => Bool -> a -> f a
orEmpty b a = if b then pure a else empty

{-# INLINE md5hex #-}
md5hex :: C.ByteString -> C.ByteString
md5hex s = convertToBase Base16 (hash s :: Digest MD5)

{-# INLINE bfs #-}
bfs :: (a -> [a]) -> a -> [a]
bfs next start = bfs' [start] []
  where
    bfs' []     [] = []
    bfs' []     ys = bfs' (reverse ys) []
    bfs' (x:xs) ys = x : bfs' xs (next x ++ ys)

applyMove :: Coord -> Move -> Coord
applyMove (Coord x y) move = case move of
  MUp    -> Coord x (y - 1)
  MDown  -> Coord x (y + 1)
  MLeft  -> Coord (x - 1) y
  MRight -> Coord (x + 1) y

encodeMove :: Move -> Char
encodeMove MUp    = 'U'
encodeMove MDown  = 'D'
encodeMove MLeft  = 'L'
encodeMove MRight = 'R'

encodePath :: [Move] -> String
encodePath = reverse . map encodeMove

unlocked :: C.ByteString -> [Move]
unlocked key = case (C.unpack . C.take 4 . md5hex) key of
  [u,d,l,r] -> catMaybes
    [ orEmpty (isOpen u) MUp
    , orEmpty (isOpen d) MDown
    , orEmpty (isOpen l) MLeft
    , orEmpty (isOpen r) MRight
    ]
  _ -> error "impossible"
  where isOpen = (`elem` "bcdef")

inGrid :: Coord -> Bool
inGrid (Coord x y) = inRange grid x && inRange grid y
  where grid :: Range Int
        grid = SpanRange 0 3

nextMoves :: Position -> [Position]
nextMoves Position{..} =
  [ Position (C.snoc key (encodeMove move)) (move:path) nextCoord
  | coord /= vault
  , move <- unlocked key
  , let nextCoord = applyMove coord move
  , inGrid nextCoord
  ]

vault :: Coord
vault = Coord 3 3

initialPosition :: String -> Position
initialPosition key = Position (C.pack key) [] (Coord 0 0)

example :: Position
example = initialPosition "hijkl"

input :: Position
input = initialPosition "qzthpkfp"

findVault :: Position -> Maybe [Move]
findVault = fmap path . find ((== vault) . coord) . bfs nextMoves

allPaths :: Position -> [[Move]]
allPaths = map path . filter ((== vault) . coord) . bfs nextMoves

main :: IO ()
main = do
  print . map (encodePath . path) $ bfs nextMoves example
  print $ findVault example
  print $ encodePath <$> findVault input
  print . length . last . allPaths $ input
