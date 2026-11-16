{-# LANGUAGE EmptyDataDeriving #-}
-- cover all cases!
 {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use second" #-}
import Data.List


-- Контролното
bellTriangle :: Int -> [[Int]]
bellTriangle 1 = [[1]]
bellTriangle n = prev ++ [foldl (\acc x -> acc ++ [last acc + x]) [lastEl] lastRow]
    where 
        prev = bellTriangle (n-1)
        lastRow = last prev
        lastEl = last lastRow

type Student = (String, Int)

scoreToGrade :: Student -> Student
scoreToGrade (n, s) 
 | s <= 29 = (n, 2)
 | s <= 59 = (n, 3)
 | s <= 69 = (n, 4)
 | s <= 79 = (n, 5)
 | otherwise = (n, 6)

sortByScore :: [Student] -> [Student]
sortByScore s = sortBy (\x y -> if snd x > snd y then GT else LT) s

filterByScore :: Int -> [Student] -> [Student]
filterByScore n s = filter (\x -> snd x > n) s

alterScores :: [Int -> Int] -> [Student] -> [Student]
alterScores fs ss = map (\(n, s) -> (n, f s)) ss
    where f = foldl1 (\acc g -> g . acc) fs

applyModifiers :: [(Int -> Bool, Int -> Int)] -> [Student] -> [Student]
applyModifiers fs ss = map (\(n, s, _) -> (n, s)) $ map (\(n, s) -> foldl (\(n, cs, s) (p, f) -> if p s then (n, f cs, s) else (n, cs, s)) (n, s, s) fs) ss

-- List comprehension

-- Да се напише функция, която приема списък и връща списъка от квадратите на числата
squared :: [Int] -> [Int]
squared l = [x ^ 2| x <- l]
-- Да се напише функция, която извежда всички възможни наредени двойки от два списъка
allPairs :: [a] -> [b] -> [(a, b)]
allPairs l1 l2 = [(x, y) | x <- l1, y <- l2]
-- Да се напише функция, която извежда всички наредени двойки (a, b), където а > b
allPairsBigger :: Ord a => [a] -> [a] -> [(a, a)]
allPairsBigger l1 l2 = [(x, y) | x <- l1, y <- l2, x > y]

-- Да се дефинира тип за двоично дърво
data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

-- Да се напише функция, която връща корена на дървото (като Node и като стойност)
getRootNode :: BTree a -> BTree a 
getRootNode (Node a _ _) = Node a Empty Empty
getRootNode Empty = Empty

getRootValue :: BTree a -> Maybe a
getRootValue (Node a _ _) = Just a
getRootValue Empty = Nothing

-- Да се напише функция, която връща стойностите по листата на дървото
getLeafs :: BTree a -> [a]
getLeafs Empty = []
getLeafs (Node a Empty Empty) = [a]
getLeafs (Node _ l r) = getLeafs l ++ getLeafs r

-- Да се напише функция, която приема число n и връща n-тия ред стойности от дървото
getRow :: Int -> BTree a -> [a]
getRow _ Empty = []
getRow 0 (Node a _ _) = [a]
getRow n (Node _ l r) = getRow (n-1) l ++ getRow (n-1) r

-- Да се дефинира функция, която fold-ва дърво с функция (DFS)
foldTree :: (a -> BTree a -> a) -> a -> BTree a -> a 
foldTree f acc Empty = f acc Empty
foldTree f acc t@(Node _ l r) = foldTree f (foldTree f (f acc t) l) r 

testFunc :: Int -> BTree a -> Int
testFunc a Empty = a - 1
testFunc a (Node _ _ _) = a + 1