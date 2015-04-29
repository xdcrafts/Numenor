module Numeric.Integration.Simpson (
    integrate,
    integrateFromN,
    integrateWithAccuracy
) where
 
-- function that splits tuple by odd and even components
splitOddsAndEvens :: [a] -> ([a], [a])
splitOddsAndEvens [] = ([], [])
splitOddsAndEvens [x] = ([x], [])
splitOddsAndEvens (x:y:xs) = (x:xp, y:yp) where (xp, yp) = splitOddsAndEvens xs
 
-- http://en.wikipedia.org/wiki/Simpson%27s_rule
-- f - function
-- a - begining of interval
-- b - end of interval
-- n - number of intervals of size h
integrate :: Fractional f => Int -> (f -> f) -> f -> f -> f
integrate n f a b =
    h / 3 * (fx 0 + 4 * sum (map fx odds) + 2 * sum (map fx evens) +  fx (fromIntegral (2 * n + 2)))
    where
        h = (b - a) / fromIntegral (2 * n)
        x i = a + h * i
        fx = f . x
        odds = fst oddsAndEvens
        evens = snd oddsAndEvens
        oddsAndEvens = splitOddsAndEvens (map fromIntegral [1..2 * n - 1])
 
-- http://en.wikipedia.org/wiki/Simpson%27s_rule
-- e - accuracy
-- f - function
-- a - begining of interval
-- b - end of interval
-- n - number of intervals of size
integrateFromN :: (Fractional f, Ord f) => f -> Int -> (f -> f) -> f -> f -> f
integrateFromN e n f a b
    | accuracy < e = solution2
    | otherwise = solution1
    where
        accuracy = abs (solution2 - solution1)
        solution1 = integrate n f a b
        solution2 = integrate (2 * n) f a b
 
-- http://en.wikipedia.org/wiki/Simpson%27s_rule
-- e - acuracy
-- f - function
-- a - begining of interval
-- b - end of inteval
integrateWithAccuracy :: (Fractional f, Ord f) => f -> (f -> f) -> f -> f -> f
integrateWithAccuracy e = integrateFromN e 10
