{-# LANGUAGE DefaultSignatures #-}
module MyArrayIx where

class Ord a => Ix a where
  range :: (a, a) -> [a]
  default range :: Enum a => (a, a) -> [a]
  range (l, r) = [l..r]

  index :: (a, a) -> a -> Int
  default index  :: Enum a => (a, a) -> a -> Int
  index (l, r) x
    | inRange (l, r) x =
      let int_x = (fromEnum x)::Int
          int_l = (fromEnum l)::Int in
      if int_l <= int_x then int_x - int_l
      else (maxBound - int_l) - (minBound - int_x)
    | otherwise = error "Index out of range"

  inRange :: (a, a) -> a -> Bool
  inRange (l, r) x = x >= l && x <= r

  rangeSize :: (a, a) -> Int
  rangeSize = length.range

instance Ix Char
instance Ix Int
instance Ix Integer
instance (Ix a, Ix b) => Ix (a,b) where
  range (l, r) = [(l_, r_) | l_ <- range (fst l, fst r),
                             r_ <- range (snd l, snd r)]
  index (l, r) x = li * (rangeSize (snd l, snd r)) + ri
    where li = index (fst l, fst r) $ fst x
          ri = index (snd l, snd r) $ snd x

