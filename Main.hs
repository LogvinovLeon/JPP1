import Graph
import System.Environment
import Control.Applicative ((<$>))

readContents :: [String] -> IO String
readContents [] = getContents
readContents (filename:_) = readFile filename

solve :: String -> String
solve c = show $ reachable (readGraph c) 1

main :: IO ()
main = solve <$> (getArgs >>= readContents) >>= putStrLn

