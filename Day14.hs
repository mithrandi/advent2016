module Main where

import           Criterion.Main
import           Crypto.Hash
import           Data.ByteArray.Encoding
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Lex.Integral
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable

md5hex :: C.ByteString -> C.ByteString
md5hex s = convertToBase Base16 (hash s :: Digest MD5)

hashes :: (C.ByteString -> C.ByteString) -> C.ByteString -> [(Int, C.ByteString)]
hashes h salt = map (\i -> (i, h (salt <> fromJust (packDecimal i)))) [0..]

stretch :: Int -> C.ByteString -> C.ByteString
stretch rounds s = iterate md5hex s !! (rounds + 1)

three :: C.ByteString -> Maybe Char
three = three' . C.unpack
  where three' (a:s@(b:c:_)) = if a == b && b == c then Just a else three' s
        three' _ = Nothing

key :: [(Int, C.ByteString)] -> Bool
key (h:hs) = case three (snd h) of
  Just c -> any ((C.replicate 5 c `C.isInfixOf`) . snd) (take 1000 hs)
  Nothing -> False

keys :: [(Int, C.ByteString)] -> [(Int, C.ByteString)]
keys hs = map head . filter key $ tails hs

main :: IO ()
main = do
  --print $ keys (hashes md5hex "ahsbgdzn") !! 63
  print $ keys (hashes (stretch 2016) "ahsbgdzn") !! 63
