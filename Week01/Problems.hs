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

-- Какво представляват функционалните езици?
-- Какво представлява Haskell
-- Каква е разликата между дефиниция и декларация на функция

-- 1) Декларации на функции
-- Задача 1: Да се декларира функция, която приема едно цяло число и връща цяло число
func1 :: Int -> Int
func1 = undefined 

-- Задача 2: Да се декларира функция, която приема две цели числа и връща цяло число
func2 :: Int -> Int -> Int 
func2 = undefined

-- Задача 3: Да се декларира функция, която приема булева стойност, символ и символен низ и връща дробно число 
func3 :: Bool -> Char -> String -> Double 
func3 = undefined

-- 2) Дефиниции на прости функции
-- Задача 1: Да се декларира и дефинира функция, която приема цяло число и връща същото число, събрано с единица
add1 :: Int -> Int 
add1 a = a + 1 

-- Задача 2: Да се напише функция, която събира две цели числа
add :: Int -> Int -> Int 
add a b = a + b

-- Задача 3: Да се напише функция, която проверява дали подадено число е равно на 1, 3 или 5
-- Pattern match
-- _ наричаме анонимна променлива
check135 :: Int -> Bool 
check135 1 = True 
check135 3 = True 
check135 5 = True 
check135 _ = False

-- Задача 4: Да се напише функция, която дели две числа. Приемаме, че ако делителят е 0, то резултата е 0
div1 :: Int -> Int -> Int
div1 _ 0 = 0
div1 a b = div a b

-- Задача 5: Фибоначи
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Задача 6: Факториел
fac :: Int -> Int
fac 0 = 1 
fac n = n * fac (n-1)

-- Задача 7: Проверка дали число е просто
isPrime :: Int -> Bool 
isPrime 1 = False
isPrime n = helper (div n 2)
    where 
        helper :: Int -> Bool
        helper 1 = True 
        helper a = if rem n a == 0 then 
                        False 
                    else helper (a-1) 

-- 3) Списъци
-- Задача 1: Да се дефинира списък с числата от 1 до 5 (по два начина)
list15' :: [Int]
list15' = [1,2,3,4,5]

list15 :: [Int]
list15 = [1..5]

-- Задача 2: Да се напише функция, която приема две цели числа (a,b) и връща списък [a, a+2, ..., b или b+1]
constructList :: Int -> Int -> [Int]
constructList a b = if a >= b then [a] else a : constructList (a+2) b  

-- Задача 3: Да се напише функция, която приема списък от цели числа и връща сумата им
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- Задача: Сума от индекс
sumFromIndex :: [Int] -> Int -> Int 
sumFromIndex [] _ = 0
sumFromIndex xs 0 = sum' xs
sumFromIndex (_ : xs) n = sumFromIndex xs (n-1)

-- Задача: Сума от индекс до индекс
sumFromToIdx :: [Int] -> Int -> Int -> Int 
sumFromToIdx [] _ _ = 0
sumFromToIdx xs 0 m = sumFromIndex (reverse xs) (length xs - 1 - m)
sumFromToIdx (_ : xs) n m = sumFromToIdx xs (n-1) (m-1)

-- Задача 4: Да се напише функция, която приема списък от цели числа и прилага следните операции:
 -- Първото число се добавя към резултата до момента (Първоначално 0)
 -- Второто число се изважда от резултата до момента
 -- Резултата до момента се умножава по третото число
 -- Резултата до момента се дели на четвъртото число
 -- Стъпките се повтарят до края на списъка и се връща резултата

funSum :: [Int] -> Int 
funSum ys = helper ys 0 1
    where 
        helper :: [Int] -> Int -> Int -> Int 
        helper [] res _ = res
        helper (x : xs) res 1 = helper xs (res + x) 2  
        helper (x : xs) res 2 = helper xs (res - x) 3
        helper (x : xs) res 3 = helper xs (res * x) 4
        helper (x : xs) res 4 = helper xs (div res x) 1
        helper _ _ _ = undefined -- fixes pattern match warning

-- Задача 5: Да се напише функция, която приема число и създава безкраен списък от това число
infList :: Int -> [Int]
infList a = [a, a..]

-- Задача 6: Да се напише функция, която приема произволна стойност и създава безкраен списък от нея
infList2 :: Enum a => a -> [a]
infList2 a = [a, a..]

-- Задача 7: Да се напише функция, която приема списък и число n и връща първите n елемента от списъка 
getN :: [a] -> Int -> [a]
getN [] _ = []
getN _ 0 = []
getN (x : xs) n = x : getN xs (n-1)

-- Задача 8: Да се напише функция, която приема списък и число n и връща последните n елемента от списъка 
getLastN :: [a] -> Int -> [a]
getLastN xs n = reverse (getN (reverse xs) n)

-- Задача 9: Да се напише функция, която приема стойност и цяло число (n) и създава списък, който съдържа стойността n пъти
repeatN :: a -> Int -> [a]
repeatN _ 0 = []
repeatN a n = a : repeatN a (n-1)
