module Main where

import Control.Monad

import CPU

testFail :: String -> IO ()
testFail = error

main :: IO ()
main = unless testAdcImm (testFail "Test ADC with immediate value failed")
