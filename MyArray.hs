module MyArray (Ix, range, rangeSize,
               Array, listArray, (!), elems, array, update, (//)) where
import MyArrayIx

data Node i e = Node {key::i, value::e, left::Node i e, right::Node i e}
              | Nil deriving (Show, Ord, Eq)

listToNode :: Ix i => [(i, e)] -> Node i e
listToNode [] = Nil
listToNode x = Node {key=k, value=v, left=listToNode l, right = listToNode r}
  where (l, (k, v):r) = splitAt (length x `div` 2) x

find :: Ix i => Node i e -> i -> Maybe e
find Nil _ = Nothing
find Node {key=k, value=v, left=l, right=r} i
  | i < k = find l i
  | i > k = find r i
  | otherwise = Just v

values :: Ix i => Node i e -> [e]
values Nil = []
values Node {value=v, left=l,right=r} = values l ++ v : values r

modify :: Ix i => i -> e -> Node i e -> Node i e
modify _ _ Nil = error "Key not found"
modify i e n@Node {key=k, left=l, right=r}
  | i < k = n {left=modify i e l}
  | i > k = n {right=modify i e r}
  | otherwise = n {value=e}

data Array i e = Array {bounds::(i, i), tree::Node i e} deriving (Show)

listArray :: Ix i => (i, i) -> [e] -> Array i e
listArray (l, r) a = Array {bounds = (l, r), tree = t}
  where t = listToNode $ zip (range (l, r)) a

(!) :: Ix i => Array i e -> i -> e
(!) Array {tree=t} i = fromJust $ find t i

elems :: Ix i => Array i e -> [e]
elems Array {tree=t} = values t

array :: Ix i => (i, i) -> [(i, e)] -> Array i e
array (l, r) a = Array {bounds=(l, r), tree=listToNode a}

update :: Ix i => i -> e -> Array i e -> Array i e
update i e a@Array {tree=t} = a {tree=modify i e t}

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
(//) = foldr $ uncurry update
-- TODO Implement faster solution

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Nothing here"

