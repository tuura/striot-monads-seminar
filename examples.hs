import Control.Monad.Reader
import Control.Monad.Random.Lazy

type Bracelet = [Bool]

random

xor :: Bool -> Bool -> Bool
True  `xor` False = True
False `xor` True  = True
_     `xor` _     = False

rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft (x:xs) = xs ++ [x]

testBracelet :: Bracelet -> Int
testBracelet xs = length . filter id $ zipWith xor xs (rotateLeft xs)

main :: IO ()
main = putStrLn "Hello World!"