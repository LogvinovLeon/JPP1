module MyArray where
import MyArrayIx

data Node i e = Node {key::i, value::e, left::Node i e, right::Node i e}
              | Nil deriving (Show, Ord, Eq)

listToNode :: Ix i => [(i, e)] -> Node i e
listToNode [] = Nil
listToNode x = Node {key=k, value=v, left=listToNode l, right = listToNode r}
  where (l, (k, v):r) = splitAt ((length x) `div` 2) x

find :: Ix i => i -> Node i e -> Maybe e
find _ Nil = Nothing
find i Node {key=k, value=v, left=l, right=r}
  | i < k = find i l
  | i > k = find i r
  | otherwise = Just v

values :: Ix i => Node i e -> [e]
values Nil = []
values Node {value=v, left=l,right=r} = values l ++ v : values r

data Array i e = Array {bounds::(i, i), tree::(Node i e)} deriving (Show)

listArray :: Ix i => (i, i) -> [e] -> Array i e
listArray (l, r) a = Array {bounds = (l, r), tree = t}
  where t = listToNode $ zip (range (l, r)) a

(!) :: Ix i => Array i e -> i -> e
(!) Array {tree=t} i = fromJust $ find i t

elems :: Ix i => Array i e -> [e]
elems Array {tree=t} = values t

array :: Ix i => (i, i) -> [(i, e)] -> Array i e
array = undefined

update :: Ix i => i -> e -> Array i e -> Array i e
update = undefined

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
(//) = undefined

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Nothing here"
