{-# LANGUAGE RecordWildCards #-}
module MyArray (Ix(..), Array, listArray, (!), elems, update, array, (//)) where

import MyArrayIx

data Node i e = Node {key::i,
                     value::e,
                     left::Node i e,
                     right::Node i e,
                     height::Int} | Nil deriving (Show, Ord, Eq)

singleton :: Ix i => (i, e) -> Node i e
singleton (i, e) = Node{key=i, value=e, left=Nil, right=Nil, height=1}

getHeight :: Ix i => Node i e -> Int
getHeight Nil = 0
getHeight Node{..} = height

balance :: Ix i => Node i e -> Int
balance Nil = 0
balance Node{..} = getHeight right - getHeight left

fixHeight :: Ix i => Node i e -> Node i e
fixHeight Nil = Nil
fixHeight n@Node{..} = n{height=succ $ maximum $ fmap getHeight [left, right]}

rLeft :: Ix i => Node i e  -> Node i e
rLeft p@Node{left=l, right=q@Node{left=m, right=r}} =
  fixHeight q{left=fixHeight p{left=l, right=m}, right=r}
rLeft _ = error "Invalid left rotation"

rRight :: Ix i => Node i e -> Node i e
rRight p@Node{left=q@Node{left=l, right=m}, right=r} =
  fixHeight q{left=l, right=fixHeight p{left=m, right=r}}
rRight _ = error "Invalid right rotation"

rebalance :: Ix i => Node i e -> Node i e
rebalance n@Node{..}
  | b == 2 = rLeft n{right=if balance right < 0 then rRight right else right}
  | b == -2 = rRight n{left=if balance left > 0 then rLeft left else left}
  | otherwise = n
   where b = balance n
rebalance Nil = error "Invalid rebalance"

add :: Ix i => (i, e) -> Node i e -> Node i e
add e Nil = singleton e
add e@(k, v) n@Node{..}
  | key < k = rebalance.fixHeight $ n{right=add e right}
  | key > k = rebalance.fixHeight $ n{left=add e left}
  | otherwise = n{value=v}

find :: Ix i => Node i e -> i -> Maybe e
find Nil _ = Nothing
find Node{..} i
  | i < key = find left i
  | i > key = find right i
  | otherwise = Just value

values :: Ix i => Node i e -> [e]
values Nil = []
values Node{..} = values left ++ [value] ++ values right

data Array i e = Array{bounds::(i, i), tree::Node i e} deriving (Show)

listArray :: Ix i => (i, i) -> [e] -> Array i e
listArray b es = Array{bounds=b, tree=foldr add Nil (zip (range b) es)}

(!) :: Ix i => Array i e -> i -> e
(!) Array{..} i
  | inRange bounds i = fromJust $ find tree i
  | otherwise = indexOutOfRange

elems :: Ix i => Array i e -> [e]
elems Array{..} = values tree

array :: Ix i => (i, i) -> [(i, e)] -> Array i e
array b es
  | all (inRange b) (map fst es) = Array{bounds=b, tree=foldr add Nil es}
  | otherwise = indexOutOfRange

update :: Ix i => i -> e -> Array i e -> Array i e
update i e a@Array{..}
  | inRange bounds i = a{tree=add (i, e) tree}
  | otherwise = indexOutOfRange

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
(//) a@Array{..} es
  | all (inRange bounds) (map fst es) = a{tree=foldr add tree es}
  | otherwise = indexOutOfRange

fromJust :: Maybe e -> e
fromJust (Just e) = e
fromJust Nothing = error "Element with this index is not defined"

indexOutOfRange :: a
indexOutOfRange = error "Index out of defined range"
