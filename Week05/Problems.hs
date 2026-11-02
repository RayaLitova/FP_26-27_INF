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
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}

-- curry and uncurry
curry' :: ((a, b) -> c) -> a -> b -> c 
curry' = undefined 

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' = undefined

replicatePairs :: [(Int, a)] -> [[a]]
replicatePairs = undefined 

sumLists :: [Int] -> [Int] -> [Int]
sumLists = undefined

filterPairs :: (a -> b -> Bool) -> [(a, b)] -> [(a, b)]
filterPairs = undefined

zipWith' :: ((a, b) -> c) -> [a] -> [b] -> [c]
zipWith' = undefined 

--

unzipWith :: (c -> (a, b)) -> [c] -> ([a], [b])
unzipWith = undefined

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' = undefined

-- Maybe
filterMaybe :: (a -> Maybe b) -> [a] -> [b]
filterMaybe = undefined

foldMaybe :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybe = undefined


-- Either
mapLeft  :: (a -> c) -> Either a b -> Either c b
mapLeft = undefined
mapBoth  :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth = undefined