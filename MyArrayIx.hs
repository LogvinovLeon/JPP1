{-# LANGUAGE DefaultSignatures #-}
module MyArrayIx where

class Ord a => Ix a where
  -- |range (l, r)
  -- returns a list of all indexes between l and r
  range :: (a, a) -> [a]
  default range :: Enum a => (a, a) -> [a]
  range (l, r) = if l <= r then [l..r] else [r..l]

  -- |index (l, r) i
  -- gives you a 0-indexed position of index i in index range (l, r)
  -- for example:
  -- index (7, 9) 8 = 1
  -- index ('a', 'd') 'd' = 3
  -- throws if index is out of range
  index :: (a, a) -> a -> Int
  default index  :: Enum a => (a, a) -> a -> Int
  index (l, r) x
    | inRange (left, right) x =
      let int_x = (fromEnum x)::Int
          int_l = (fromEnum left)::Int in
      if int_l <= int_x then int_x - int_l
      else (maxBound - int_l) - (minBound - int_x)
    | otherwise = error "Index out of range"
      where left = min l r
            right = max l r

  -- |inRange (l, r) i
  -- checks if index i is in range (l, r)
  inRange :: (a, a) -> a -> Bool
  inRange (l, r) x = x >= (min l r) && x <= (max l r)

  -- |rangeSize (l, r)
  -- returns the size of the range (l, r)
  rangeSize :: (a, a) -> Int
  rangeSize (l, r) = length $ range (l, r)

instance Ix Char

instance Ix Int

instance Ix Integer
-- TODO: Get rid of overflow in fromEnum

instance (Ix a, Ix b) => Ix (a,b) where
  range (l, r) = [(l_, r_) | l_ <- range (fst l, fst r),
                             r_ <- range (snd l, snd r)]
  index (l, r) x = li * (rangeSize (snd l, snd r)) + ri
    where li = index (fst l, fst r) $ fst x
          ri = index (snd l, snd r) $ snd x
