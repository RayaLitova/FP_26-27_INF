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

-- :t <function>
-- :info <type>

-- Още малко работа със списъци

-- Да се напише функция, която обръща списък
-- Вградена функция: reverse
reverse' :: [a] -> [a]
reverse' [] = [] 
reverse' (x : xs) = reverse' xs ++ [x] 

-- Да се напише функция, която връща дължината на списък
-- Вградена функция: length
length' :: [a] -> Int 
length' [] = 0
length' (_ : xs) = 1 + length' xs 

-- Да се напише функция, която проверява дали елемент се съдържа в списък
-- Вградена функция: elem
elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False 
elem' a (x : xs) = a == x || elem' a xs 

-- Да се напише функция, която проверява дали всички елементи от един списък се съдържат във втори
elem2 :: Eq a => [a] -> [a] -> Bool 
elem2 [] _ = True
elem2 (x : xs) ys = elem' x ys && elem2 xs ys

-- Да се напише функция, която приема два списъка и ги превръща в списък от наредени двойки. В i-тата наредена двойка се съдържат елементите на двата масива, които се намират на индекс i
-- Вградена функция: zip 
zip' :: [a] -> [a] -> [(a, a)]
zip' [] _ = [] 
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- Функции от по-висок ред

-- Да се напише функция, която приема цяло число и функция Int->Int и прилага функцията върху числото
applyInt :: Int -> (Int -> Int) -> Int 
applyInt a f = f a 

-- Да се напише функция, която приема списък и функция и прилага функцията върху всеки елемент на списъка
-- Вградена функция: map
map' :: [a] -> (a -> b) -> [b]
map' [] _ = []
map' (x : xs) f = f x : map' xs f

-- Да се напише функция, която приема списък, функция и предикат. Тя трябва да приложи първата функция върху всички елементи, чиито индекси удовлетворяват предиката.
mapSome :: [a] -> (a -> a) -> (Int -> Bool) -> [a]
mapSome as f1 p = helper as f1 0
    where 
        helper :: [a] -> (a -> a) -> Int -> [a]
        helper [] _ _ = []
        helper (x : xs) f i = if p i then f x : helper xs f (i+1) else x : helper xs f (i+1) 


-- Да се напише функция, която приема списък и предикат. Тя трябва да върне всички елементи до първия, който не удовлетворява предиката
-- Вградена функция: takeWhile 
-- dropWhile - същото, но изпуска елементите
takeWhile' :: [a] -> (a -> Bool) -> [a]
takeWhile' [] _ = []
takeWhile' (x : xs) p = if p x then x : takeWhile' xs p else []

-- Да се напише функция, която приема два списъка и функция и връща нов списък, 
-- получен чрез прилагане на функцията върху елементите на един и същ индекс.
-- Вградена функция: zipWith
zipWith' :: [a] -> [b] -> (a -> b -> c) -> [c]
zipWith' [] _ _ = []
zipWith' _ [] _ = []
zipWith' (x : xs) (y : ys) f = f x y : zipWith' xs ys f

-- Дефиниране на типове
-- data vs type vs newtype
-- Record
-- На който му е интересно: https://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell

-- Да се дефинира тип за вид на животно
data AnimalType = Dog | Cat | Parrot | Cow | Lion
    deriving (Show, Read, Eq) 

-- Да се дефинира тип, който представлява наредена двойка от животно и неговото име
type Animal = (AnimalType, String)

-- Да се дефинира тип за човек. Всеки човек има име, възраст и домашен любимец
data Person = Male {name :: String, age :: Int, pet :: Animal} | Female {name :: String, pet :: Animal}
    deriving Show

-- Да се дефинира тип Zoopark, който представлява списък от животни
type Zoopark = [Animal] 

-- Да се напише функция, която брои колко лъва има в един зоопарк
countLions :: Zoopark -> Int 
countLions [] = 0 
countLions ((t, _) : xs) = if t == Lion then 1 + countLions xs else countLions xs 

-- Да се дефинира тип за двумерна матрица
type Matrix a = [[a]]

-- Отговор на въпрос, зададен по време на упражнението:
-- Не можем да добавяме constraints за типа а в type. Можем в data, но е лоша практика.
-- https://stackoverflow.com/questions/40825878/type-constraints-in-data-declaration-haskell

-- Да се напише функция, която транспонира матрица
transpose' :: Matrix a -> Matrix a 
transpose' [] = []
transpose' ([] : _) = []
transpose' m = map head m : transpose' (map tail m)

-- За домашно: Да се напише функция, която умножава две матрици
-- Упътване: Използвайте транспонираната матрица.
