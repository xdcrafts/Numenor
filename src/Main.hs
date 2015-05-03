module Main where

import Numeric.Integration.Simpson as S
import Numeric.Integration.Gauss as G

f :: Double -> Double
f x = x ** 5

f4 :: Double -> Double
f4 x = 120 * x

integratedF :: Double -> Double
integratedF x = x ** 6 / 6

a :: Double
a = 1

b :: Double
b = 2

accuracy :: Double
accuracy = 0.000001

main::IO()
main = do
    putStrLn $ "integrated with simpson: " ++ show (S.integrateE accuracy f a b)
    putStrLn $ "integrated with gauss: " ++ show (G.integrate accuracy 16 f a b)
    putStrLn $ "accurate value is: " ++ show (integratedF b - integratedF a)