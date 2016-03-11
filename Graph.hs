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
readGraph s = array bounds $ sort $ readIncidence <$> ls
  where ls = lines s; bounds = (1, length ls)

graphSize :: Graph -> Int
graphSize = length.elems

type Used = Array Vertex Bool

dfs :: Graph -> Vertex -> Used -> Used
dfs g v used =
  if used ! v then used
  else foldr (dfs g) (used // [(v, True)]) (g ! v)

reachable :: Graph -> Vertex -> [Vertex]
reachable g v = let gs = graphSize g in
                let used = array (1, gs) (zip [1..gs] (cycle [False])) in
                let usage = zip [1..gs] (elems $ dfs g v used) in
                fst <$> (filter snd usage)

