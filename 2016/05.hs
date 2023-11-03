{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash
import Data.ByteString qualified as B
import Data.List
import Data.String

type ID = B.ByteString

type Index = Int

md5 :: (ID, Index) -> Digest MD5
md5 (id, i) = hash (id <> fromString (show i))

interesting :: (ID, Index) -> Bool
interesting pair = all (== '0') x
  where
    x = take 5 $ show $ md5 pair

interesting' :: String -> Bool
interesting' d = all (== '0') x && elem y ['0' .. '7']
  where
    x = take 5 d
    y = d !! 5

findChars :: [(ID,Index)] -> Int -> [(Char,Char)]
findChars (p:ps) n = go n (p:ps) []
  where
    go :: Int -> [(ID,Index)] -> [(Char,Char)] -> [(Char,Char)]
    go n (p:ps) acc
      | length acc == n = acc
      | otherwise =
          case (interesting' h, i `notElem` (fst <$> acc)) of
            (True, True) -> go n ps ((i,c):acc)
            _            -> go n ps acc
      where
        h = show (md5 p)
        i = h !! 5
        c = h !! 6

decrypt :: [(Char,Char)] -> String
decrypt = fmap snd . sortOn fst

-- I had to cheat on this one by increasing the stack size
-- I'm not sure why it uses so much memory, the go function in
-- findchars only have tail calls, so that shouldn't be the problem
main :: IO ()
main = do
  let id = "uqwqemis"
  let pairs = zip (repeat id) [1 ..]
  let len = B.length id
  let a = take len (filter interesting pairs)
  print $ (!! 5) . show . md5 <$> a
  print $ decrypt (findChars pairs 8)
