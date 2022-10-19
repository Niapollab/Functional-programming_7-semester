module Task1(solveQuadraticEquation, sumAndCount, removeCapitalizedWords, sumOfFirst, deleteEach, maxIncreasingSubsequence, permutations) where

import Data.Char(isUpper, isLower)

-- 1
--
discriminant :: Num a => a -> a -> a -> a
discriminant a b c = b ^ 2 - 4 * a * c

solveQuadraticEquation :: (Floating a, Real a) => a -> a -> a -> (a, Maybe a)
solveQuadraticEquation 0 0 _ = error "Infinity solutions count."
solveQuadraticEquation 0 b c = (-c / b, Nothing)
solveQuadraticEquation a b c = ((b + sqrt d) / 2 * a, Just ((b - sqrt d) / 2 * a))
    where
        d = discriminant a b c

-- 2
--
digits :: Integral a => a -> [a]
digits 0 = []
digits number = digits (div number 10) ++ [mod number 10]

sumAndCount :: Integral a => a -> (a, Int)
sumAndCount number = (sum d, length d)
    where
        d = digits (abs number)

-- 3
--
removeCapitalizedWords :: [Char] -> String
removeCapitalizedWords text = unwords (filter cond w)
    where
        cond = all isLower
        w = words text

-- 4
--
sumOfFirst :: (Fractional a, Eq a, Enum a) => a -> a
sumOfFirst 0 = 0
sumOfFirst count = sum (map mutator [1..count])
    where
        mutator x = x / (count ^ 2)

-- 5
--
deleteEach :: Integral a => a -> [b] -> [b]
deleteEach index elements = map snd (filter cond pairs)
    where
        cond element = (mod (fst element) index) /= 0
        pairs = zip [1..] elements

-- 6
--
takeWhileIncreasing :: (Integral a) => [a] -> ([a], [a])
takeWhileIncreasing [] = ([], [])
takeWhileIncreasing elements = (increasing, notIncreasing)
    where
        increasing = if length elements > 1
            then
                findIncreasing elements
            else
                elements
        notIncreasing = drop (length increasing) elements
        findIncreasing (x:tail) = if x <= head tail
            then
                if length tail == 1
                    then
                        x:tail
                    else
                        x:findIncreasing tail
            else
                [x]

increasingSubsequence :: Integral a => [a] -> [[a]]
increasingSubsequence [] = []
increasingSubsequence elements = if length tail > 0
    then
        increasing:increasingSubsequence tail
    else
        [increasing]
    where
        increasing = fst subsets
        tail = snd subsets
        subsets = takeWhileIncreasing elements

maxIncreasingSubsequence :: Integral a => [a] -> [[a]]
maxIncreasingSubsequence elements = filter cond sequences
    where
        cond element = length element == maxElementsCount
        maxElementsCount = maximum (map length sequences)
        sequences = increasingSubsequence elements

-- 7
--
delete :: Eq a => a -> [a] -> [a]
delete value elements = firstPart ++ secondPart
    where
        secondPart = drop (length firstPart + 1) elements
        firstPart = takeWhile (/= value) elements

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations elements = [x:tail | x <- elements, tail <- permutations (delete x elements)]