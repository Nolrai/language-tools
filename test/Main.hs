module Main (tests) where

import Test.Tasty (defaultMain)
import Test.MyLib (tests)

main :: IO ()
main = defaultMain tests
