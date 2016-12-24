module Main where

import Text.Parsec

data Node = Node
            { nodeName :: String
            , nodeUsed :: Integer
            , nodeAvail :: Integer
            } deriving (Show, Eq)

name :: Stream s m Char => ParsecT s u m String
name = many1 (noneOf " ")

size :: Stream s m Char => ParsecT s u m Integer
size = read <$> many1 digit <* char 'T'

node :: Stream s m Char => ParsecT s u m Node
node = Node <$> name <* spaces <* size <* spaces <*> size <* spaces <*> size

parseNode :: String -> Node
parseNode s = case parse node "" s of
  Right n -> n
  Left err -> error (show err)

main :: IO ()
main = do
  input <- readFile "Day22.txt"
  let nodes = map parseNode . drop 2 . lines $ input
  print $ length [ (a, b)
                 | a <- nodes
                 , b <- nodes
                 , a /= b
                 , nodeUsed a > 0
                 , nodeUsed a <= nodeAvail b
                 ]
