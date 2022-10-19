module Task1(solveQuadraticEquation, sumAndCount, removeCapitalizedWords, sumOfFirst, deleteEach, maxIncreasingSubsequence, permutations) where

import Data.Char(isUpper, isLower)

-- 1
--
solveQuadraticEquation :: (Floating a, Eq a) => a -> a -> a -> (a, Maybe a)
solveQuadraticEquation 0 0 0 = error "Infinity solutions count."
solveQuadraticEquation 0 0 _ = error "No solutions."
solveQuadraticEquation 0 b c = (-c / b, Nothing)
solveQuadraticEquation a b c = ((b + sqrt d) / 2 * a, Just ((b - sqrt d) / 2 * a))
    where
        d = discriminant a b c
        discriminant a b c = b ^ 2 - 4 * a * c

-- 2
--
sumAndCount :: Integral a => a -> (a, Int)
sumAndCount number = (sum d, length d)
    where
        d = digits . abs $ number
        digits 0 = []
        digits number = number `mod` 10 : digits (number `div` 10)

-- 3
--
removeCapitalizedWords :: [Char] -> String
removeCapitalizedWords text = unwords . filter cond . words $ text
    where
        cond = all isLower

-- 4
--
sumOfFirst :: Int -> Double
sumOfFirst 0 = 0
sumOfFirst count = sum . take count . map mutator $ [1..]
    where
        mutator x = 1 / (x ^ 2)

-- 5
--
deleteEach :: Integral a => a -> [b] -> [b]
deleteEach index elements = map snd . filter cond $ zip [1..] elements
    where
        cond element = fst element `mod` index /= 0

-- 6
--
takeWhileIncreasing :: (Integral a) => [a] -> ([a], [a])
takeWhileIncreasing [] = ([], [])
takeWhileIncreasing elements = (increasing, notIncreasing)
    where
        increasing
            | length elements > 1 = findIncreasing elements
            | otherwise = elements
        notIncreasing = drop (length increasing) elements
        findIncreasing (x:tail)
            | x <= head tail = if length tail == 1
                    then
                        x:tail
                    else
                        x:findIncreasing tail
            | otherwise = [x]

increasingSubsequence :: Integral a => [a] -> [[a]]
increasingSubsequence [] = []
increasingSubsequence elements
    | length tail > 0 = increasing:increasingSubsequence tail
    | otherwise = [increasing]
    where
        increasing = fst subsets
        tail = snd subsets
        subsets = takeWhileIncreasing elements

maxIncreasingSubsequence :: Integral a => [a] -> [[a]]
maxIncreasingSubsequence elements = map fst . filter cond $ pairSequenceLength
    where
        cond x = snd x == maxElementsCount
        maxElementsCount = maximum . map snd $ pairSequenceLength
        pairSequenceLength = zip sequences . map length $ sequences
        sequences = increasingSubsequence elements

-- 7
--
delete :: Eq a => a -> [a] -> [a]
delete value elements = deleteValue elements
    where
        deleteValue [] = []
        deleteValue (x:tail)
            | x == value = tail
            | otherwise = x:deleteValue tail


permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations elements = [x:tail | x <- elements, tail <- permutations (delete x elements)]