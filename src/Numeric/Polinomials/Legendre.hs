module Numeric.Polinomials.Legendre (
    polinomial,
    polinomial',
    polinomialRoot,
    polinomialRootE,
    polinomialRoots
) where

-- | Returns function of n'th Legendre polinomial
-- n - order of polinomial
polinomial :: Int -> Double -> Double
polinomial 0 = const 1
polinomial 1 = id
polinomial n = \x -> (2 * nd + 1) / (nd + 1) * x * pn1 x - nd / (nd + 1) * pn2 x
    where nd = fromIntegral (n - 1)
          pn1 = polinomial (n - 1)
          pn2 = polinomial (n - 2)

-- | Returns first derivative of n'th Legendre polinomial
-- n - order of polinomial
polinomial' :: Int -> Double -> Double
polinomial' n = \x -> nd / (1 - x * x) * (polinomial (n - 1) x - x * polinomial n x)
    where nd = fromIntegral n

-- | Calculates k'th approximation of i'th root of n'th Legndre polinomial
-- n - order of polinomial
-- i - number of polinomial's root
-- k - order of approximation
polinomialRoot :: Int -> Int -> Int -> Double
polinomialRoot n i 0 = cos (pi * (4 * fromIntegral i - 1) / (4 * fromIntegral n + 2))
polinomialRoot n i k = xk1 - polinomial n xk1 / polinomial' n xk1
    where xk1 = polinomialRoot n i (k - 1)

-- | Calculates k'th approximation of i'th root of n'th Legndre polinomial with specified accuracy e
-- k - order of approximation
-- e - accuracy
-- n - order of polinomial
-- i - number of polinomial's root
polinomialRootEK :: Int -> Double -> Int -> Int -> Double
polinomialRootEK k e n i
    | accuracy < e = solution2
    | otherwise = solution1
    where
        accuracy = abs (solution2 - solution1)
        solution1 = polinomialRoot n i k
        solution2 = polinomialRoot n i (k + 1)

-- | Calculates i'th root of n'th Legndre polinomial with specified accuracy e
-- e - accuracy
-- n - order of polinomial
-- i - number of polinomial's root
polinomialRootE :: Double -> Int -> Int -> Double
polinomialRootE = polinomialRootEK 1

-- | Calculates roots of n'th Legndre polinomial with specified accuracy e
-- e - accuracy
-- n - order of polinomial
polinomialRoots :: Double -> Int -> [Double]
polinomialRoots e n = map root [1..n]
    where root = polinomialRootE e n