module Main where

testFail :: String -> IO ()
testFail = error

main :: IO ()
main = putStrLn "No tests yet"
