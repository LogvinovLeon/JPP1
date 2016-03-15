{-# LANGUAGE TupleSections #-}
module Graph (readGraph, reachable) where
import MyArray
import Data.List (sort)
import Control.Applicative ((<$>))

type Vertex = Int
type Incidence = (Vertex, [Vertex])
type Graph = Array Vertex [Vertex]

readIncidence :: String -> Incidence
readIncidence s = (v, neighbours)
  where v:neighbours = read <$> words s

readGraph :: String -> Graph
readGraph s = array bounds is
  where bounds = (fst $ head is, fst $ last is)
        is = sort $ readIncidence <$> lines s

graphSize :: Graph -> Int
graphSize = length.elems

type Used = Array Vertex Bool

dfs :: Graph -> Vertex -> Used -> Used
dfs g v used
  | used ! v = used
  | otherwise = foldr (dfs g) (used // [(v, True)]) (g ! v)

reachable :: Graph -> Vertex -> [Vertex]
reachable g v = let gs = graphSize g
                    used = array (1, gs) ((,False) <$> [1..gs])
                    usage = zip [1..gs] (elems $ dfs g v used) in
                fst <$> filter snd usage

