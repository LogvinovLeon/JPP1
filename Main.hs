import System.Environment
import Control.Applicative
import Graph

readContents :: [String] -> IO String
readContents [] = getContents
readContents (filename:_) = readFile filename

solve :: String -> String
solve c = let g = readGraph $ lines c in
          show $ reachable g 1

main :: IO ()
main = solve <$> (getArgs >>= readContents) >>= putStrLn

