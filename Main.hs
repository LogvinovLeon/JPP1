import Graph
import System.Environment
import qualified Data.ByteString.Char8 as B

readContents :: [String] -> IO B.ByteString
readContents [] = B.getContents
readContents (filename:_) = B.readFile filename

solve :: String -> String
solve c = show $ reachable (readGraph c) 1

main :: IO ()
main = do
  contents <- getArgs >>= readContents
  putStrLn $ solve $ B.unpack contents
