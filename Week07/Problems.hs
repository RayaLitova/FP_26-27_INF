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

-- Какво е монад?
-- Functor - fmap 
-- Applicative - (<*>)
-- Monad - (>>=)

-- Готина статия: www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

-- Какви монади познаваме? 
    -- []
    -- Maybe
    -- Either 

-- fmap == <$> (but infix)
-- Да се дефинира функция която приема число и Maybe Int. Функцията трябва да връща сумата на двете числа
functorSum :: Int -> Maybe Int -> Maybe Int 
functorSum a b = fmap (+a) b

-- <*> (apply)
-- Да се дефинира функция, която приема Maybe (Int -> Int) и Maybe Int. Тя трябва да приложи функцията върху числото
applyInt :: Maybe (Int -> Int) -> Maybe Int -> Maybe Int 
applyInt f x = f <*> x

-- Да се дефинира функция, която приема [Int -> Int] и [Int]. Тя трябва да приложи всяка функция върху всяко число
applyAll :: [Int -> Int] -> [Int] -> [Int]
applyAll fs xs = fs <*> xs

-- >>= (bind)
-- Да се дефинира функцията safeDivide 
safeDivide :: Int -> Int -> Maybe Int 
safeDivide _ 0 = Nothing
safeDivide a b = Just $ div a b

-- Да се дефинира функция, която приема Maybe Int и две числа. Тя трябва последователно да приложи safeDivide с двете числа
bindSafeDivide :: Maybe Int -> Int -> Int -> Maybe Int 
bindSafeDivide a b c = a >>= \x -> safeDivide x b >>= \y -> safeDivide y c

-- IO

-- Да се напише функция, която принтира "Hello world!" в терминала
helloWorld :: IO ()
helloWorld = print "Hello world"

-- Да се напише функция, която чете стринг от терминала, слага му "!!!" в края и го принтира
modString :: IO () 
modString = do 
    str <- getLine
    print $ str ++ "!!!"

-- Да се напише фунцкия, която чете число от терминала, добавя му 1 и го принтира
addOne :: IO () 
addOne = do
    str <- getLine 
    let a = read str 
    print $ a + 1

data ValBox a = Box a | Empty
    deriving Show

-- Да се имплементират fmap, <*> и >>=
fmapValBox :: (a -> a) -> ValBox a -> ValBox a
fmapValBox _ Empty = Empty
fmapValBox f (Box a) = Box $ f a

(<!>) :: ValBox (a -> b) -> ValBox a -> ValBox b
(<!>) Empty _ = Empty
(<!>) _ Empty = Empty
(<!>) (Box f) (Box a) = Box $ f a

(<=>=>) :: ValBox a -> (a -> ValBox b) -> ValBox b 
(<=>=>) Empty _ = Empty
(<=>=>) (Box a) f = f a 

readNum :: IO (Maybe Int) 
readNum = do 
    str <- getLine 
    let x = read str 
    if x == 0 then
        return Nothing 
    else 
        return $ Just x 

wordle :: String -> Int -> IO () 
wordle word a = do 
    str <- getLine 
    if str == word then 
        print "GJ"
    else if a == 0 then
        print "Loser :("
    else do 
        print $ map (\(x, i) -> if x == word !! i then '*' else if x `elem` word then '!' else '_') $ zip str [0..]
        wordle word (a - 1)