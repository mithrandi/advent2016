module Main where

import Control.Lens hiding ((|>))
import Data.Bool
import Data.Foldable
import Data.Sequence as S

scramble :: Seq Bool -> Seq Bool
scramble a = (a |> False) >< fmap not (S.reverse a)

generate :: Int -> Seq Bool -> Seq Bool
generate n s | S.length s >= n = S.take n s
             | otherwise      = generate n (scramble s)

unpackWith :: (a -> a -> b) -> Seq a -> b
unpackWith f s = case toList s of
                   [a,b] -> f a b
                   _ -> error "invalid"

checksum :: Seq Bool -> Seq Bool
checksum s | even (S.length s) =
               checksum $ fmap (unpackWith (==)) (S.chunksOf 2 s)
           | otherwise = s

encoded :: Iso' String (Seq Bool)
encoded = iso from' to'
  where
    to' = toList . fmap (bool '0' '1')
    from' = fromList . map (== '1')

main :: IO ()
main = do
  putStrLn "Examples"
  print $ over encoded scramble "1"
  print $ over encoded scramble "0"
  print $ over encoded scramble "11111"
  print $ over encoded scramble "111100001010"
  putStrLn "-------"
  print $ over encoded checksum "110010110100"
  print $ over encoded (checksum . generate 20) "10000"
  putStrLn "-------"
  putStrLn "Part 1"
  print $ over encoded (checksum . generate 272) "11011110011011101"
  putStrLn "-------"
  putStrLn "Part 2"
  print $ over encoded (checksum . generate 35651584) "11011110011011101"
