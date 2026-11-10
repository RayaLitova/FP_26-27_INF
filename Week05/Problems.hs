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
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Fuse foldr/map" #-}

-- curry and uncurry
curry' :: ((a, b) -> c) -> a -> b -> c 
curry' f a b = f (a, b)  

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

replicatePairs :: [(Int, a)] -> [[a]]
replicatePairs xs = map (uncurry replicate) xs 

sumLists :: [Int] -> [Int] -> [Int]
sumLists xs ys = map (uncurry (+)) $ zip xs ys

filterPairs :: (a -> b -> Bool) -> [(a, b)] -> [(a, b)]
filterPairs p xs = foldr (\x acc -> if uncurry p x then x : acc else acc) [] xs

zipWith' :: ((a, b) -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [] 
zipWith' _ _ [] = [] 
zipWith' f xs ys =  map (\(b, i) -> (fs!!i) b) $ reverse bs
    where 
        bs = foldl (\acc x -> (x, snd (last acc) + 1) : acc) [(head ys, 0)] (tail ys)
        fs = map (curry f) xs 


--

unzipWith :: (c -> (a, b)) -> [c] -> ([a], [b])
unzipWith f l = foldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], []) $ map f l

-- Maybe
filterMaybe :: (a -> Maybe b) -> [a] -> [b]
filterMaybe p xs = foldr helper [] $ map p xs
    where 
        helper Nothing acc = acc 
        helper (Just x) acc = x : acc

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f b = helper (f b)
    where 
        helper Nothing = []
        helper (Just (x, y)) = x : helper (f y)


-- Either
mapLeft  :: (a -> c) -> [Either a b] -> [Either c b]
mapLeft f xs = map helper xs
    where 
        helper (Left a) = Left (f a)
        helper (Right b) = Right b

-- domashno rekursivno i s map
mapBoth  :: (a -> c) -> (b -> d) -> [Either a b] -> [Either c d]
mapBoth = undefined