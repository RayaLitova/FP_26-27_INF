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

-- Да се дефинира функция, която приема число n и връща първите n реда от триъгълника на Паскал
pascal :: Int -> [[Int]]
pascal 0 = []
pascal 1 = [[1]]
pascal n = prev ++ [1 : zipWith (+) lastEl (tail lastEl) ++ [1]]
    where  
        lastEl = last prev
        prev = pascal (n-1)

-- Композиции на функции:
-- Да се напише функция, която приема списък и две функции (а -> a). Тя трябва да приложи двете функции върху всеки елемент
mapFs :: [a] -> (a -> a) -> (a -> a) -> [a]
mapFs l f g = map (g.f) l

-- Същата задача, но със списък от функции
mapFss :: [a] -> [a -> a] -> [a]
mapFss l fs = map (foldl1 (.) (reverse fs)) l 

-- Maybe
data MyMaybe a = MyNothing | MyJust a
    deriving Show

-- Да се напише функцията safeHead.
safeHead :: [a] -> Maybe a 
safeHead [] = Nothing 
safeHead (x : _) = Just x 

-- Да се напише функция, която приема списък и предикат. Всеки елемент от списъка, който не удовлетворява предиката, трябва да се замени с Nothing
maybeFilter :: [a] -> (a -> Bool) -> [Maybe a]
maybeFilter l p = map (\x -> if p x then Just x else Nothing) l

-- Да се напише функция, която сумира елементите на списък от Maybe Int
sumMaybe :: [Maybe Int] -> Int 
sumMaybe l = foldl helper 0 l
    where 
        helper :: Int -> Maybe Int -> Int 
        helper acc Nothing = acc 
        helper acc (Just a) = acc + a

-- Either
data MyEither a b = MyLeft a | MyRight b
    deriving Show

-- Да се напише функция, която приема два списъка и дели поелементно. 
-- Ако някъде присъства деление на 0 да се върне грешка
divLists :: [Int] -> [Int] -> Either String [Int]
divLists xs ys = if elem 0 ys then Left "Division by 0" else Right (zipWith div xs ys)

-- Да се напише функция splitEithers тя приема списък от Either a b и връща наредена двойка ([a], [b])
splitEithers :: [Either a b] -> ([a], [b])
splitEithers l = foldr helper ([], []) l 
    where 
        helper :: Either a b -> ([a], [b]) -> ([a], [b])
        helper (Left x) (xs, ys) = (x : xs, ys)
        helper (Right y) (xs, ys) = (xs, y : ys)

