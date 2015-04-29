module Main where

import Numeric.Integration.Simpson as S

main::IO()
main = putStrLn . show $ S.integrateWithAccuracy 0.001 square 1 2
