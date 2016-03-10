import System.Environment
import Control.Applicative
import Data.List
import MyArray

readContents :: [String] -> IO String
readContents [] = getContents
readContents (filename:_) = readFile filename

type Vertex = Int
type Incidence = (Vertex, [Vertex])
type Graph = Array Vertex [Vertex]

readIncidence :: String -> Incidence
readIncidence s = (v, neighbours)
  where v:neighbours = read <$> words s

readGraph :: [String] -> Graph
readGraph ls = array (1, length ls) $ sort $ map readIncidence ls

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

solve :: String -> String
solve c = let g = readGraph $ lines c in
          show $ reachable g 1

main :: IO ()
main = solve <$> (getArgs >>= readContents) >>= putStrLn
