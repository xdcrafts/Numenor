module Main where

import Numeric.Integration.Simpson

square :: Double -> Double
square x = x * x

main::IO()
main = putStrLn . show $ integrateWithAccuracy 0.001 square 1 2
