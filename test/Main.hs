module Main (main) where

import Test.Tasty (defaultMain)
import TestMyLib (tests)

main :: IO ()
main = defaultMain tests