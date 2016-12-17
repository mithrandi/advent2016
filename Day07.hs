module Main where

import Control.Lens as L
import Data.List
import Text.ParserCombinators.Parsec as P

data IPFragment = Regular { _content :: String }
                | Hypernet { _content :: String }
  deriving (Show, Eq)
makePrisms ''IPFragment

data IP = IP { _fragments :: [IPFragment] }
  deriving (Show, Eq)
makePrisms ''IP

parseIPv7 :: String -> IP
parseIPv7 s = case parse ipv7 "(unknown)" s of
  Left _ -> error "parse error"
  Right ip -> ip

ipv7 :: GenParser Char st IP
ipv7 = IP <$> many1 (Regular <$> fragment <|> Hypernet <$> hypernet)

fragment :: GenParser Char st String
fragment = many1 (P.noneOf "[]")

hypernet :: GenParser Char st String
hypernet = char '[' *> fragment <* char ']'

hasABBA :: String -> Bool
hasABBA (a:s@(b:c:d:_)) = (a /= b && [a, b] == [d, c]) || hasABBA s
hasABBA _ = False

supportsTLS :: IP -> Bool
supportsTLS ip =
  anyOf (_IP . folded . _Regular) hasABBA ip
  && L.noneOf (_IP . folded . _Hypernet) hasABBA ip

findABAs :: IP -> [String]
findABAs ip = ip ^.. _IP . folded . _Regular . to findABAs' . folded
  where findABAs' :: String -> [String]
        findABAs' (a:s@(b:c:_)) =
          if a == c && a /= b
          then [a,b,c]:findABAs' s
          else findABAs' s
        findABAs' _ = []

supportsSSL :: IP -> Bool
supportsSSL ip = any hasBAB (findABAs ip)
  where hasBAB :: String -> Bool
        hasBAB (a:b:_) = anyOf (_IP . folded . _Hypernet) ([b,a,b] `isInfixOf`) ip
        hasBAB _ = error "invalid ABA"

main :: IO ()
main = do
  putStrLn "Examples:"
  print $ supportsTLS (parseIPv7 "abba[mnop]qrst")
  print $ supportsTLS (parseIPv7 "abcd[bddb]xyyx")
  print $ supportsTLS (parseIPv7 "aaaa[qwer]tyui")
  print $ supportsTLS (parseIPv7 "ioxxoj[asdfgh]zxcvbn")
  putStrLn "-------"
  input <- map parseIPv7 . lines <$> readFile "Day07.txt"
  print . length . filter supportsTLS $ input
  putStrLn "-------"
  putStrLn "Examples:"
  print $ supportsSSL (parseIPv7 "aba[bab]xyz")
  print $ supportsSSL (parseIPv7 "xyx[xyx]xyx")
  print $ supportsSSL (parseIPv7 "aaa[kek]eke")
  print $ supportsSSL (parseIPv7 "zazbz[bzb]cdb")
  putStrLn "-------"
  print . length . filter supportsSSL $ input
