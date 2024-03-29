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
        is = sort $ readIncidence <$> filter (\line -> not (null line)) (lines s)

graphBounds :: Graph -> (Int, Int)
graphBounds g = (minimum vs, maximum vs)
  where vs = graphVertexes g

graphVertexes :: Graph -> [Int]
graphVertexes = indices

type Used = Array Vertex Bool

dfs :: Graph -> Vertex -> Used -> Used
dfs g v used
  | used ! v = used
  | otherwise = foldr (dfs g) (used // [(v, True)]) (g ! v)

reachable :: Graph -> Vertex -> [Vertex]
reachable g v
  | elem v vertexes = fst <$> filter snd (assocs $ dfs g v used)
  | otherwise = []
  where gb = graphBounds g
        vertexes = graphVertexes g
        used = array gb ((,False) <$> vertexes)

