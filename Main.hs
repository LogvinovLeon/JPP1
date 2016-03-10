import System.Environment

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

readContents :: [String] -> IO String
readContents [] = getContents
readContents (filename:_) = readFile filename

main :: IO ()
main = do getArgs >>= readContents >>= putStrLn
