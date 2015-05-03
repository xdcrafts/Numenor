module Numeric.Integration.Gauss (
    integrate
) where

import Numeric.Polinomials.Legendre

-- | Gauss integration with Legendre polinomials of n'th order
-- er - accuracy for polinomial roots
-- n - order of Legendre polinomyal
-- f - function to integrate
-- a, b - integration limits
integrate :: Double -> Int -> (Double -> Double) -> Double -> Double -> Double
integrate er n f a b = (b - a) / 2 * amount
    where amount = sum $ map step (polinomialRoots er n)
          step x = wi x * f (arg x)
          wi x = 2 / ((1 - x * x) * polinomial' n x)
          arg x = (b - a) / 2 * x + (b + a) / 2

