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
{-# HLINT ignore "Use map once" #-}

-- Да се напише функция, която приема списък от цели числа, предикат и функция
-- Списъка трябва да се филтрира чрез предиката. Накрая, върху всеки филтриран елемент трябва да се приложи функцията
func1 :: [Int] -> (Int -> Bool) -> (Int -> Int) -> [Int]
func1 xs p f = map f $ filter p xs

-- Да се напише фунцкия, която "сгъва" списък от ляво надясно с подадена функция
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc 
foldl' f acc (x : xs) = foldl' f (f acc x) xs 
-- За упражнение вкъщи: от дясно наляво (foldr)

-- Да се напише функция, която премахва повтарящи се последователни елементи от списък
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (dropWhile (==x) xs)

-- Да се напише функция, която приема списък и предикат и връща позициите на елементите, които удовлетворяват предиката
getIndices :: [a] -> (a -> Bool) -> [Int]
getIndices xs p = map fst $ filter (\(_, x) -> p x) $ zip [0..] xs

-- Да се напише функция, която приема списък от цели числа и връща резултата чрез прилагане последователно на + и - 
cycleSum :: [Int] -> Int 
cycleSum [] = 0
cycleSum (x : xs) = foldl (\acc (f, a) -> f acc a) x $ zip (cycle [(+), (-)]) xs

-- Да се напише функция, която приема списък от цели числа и връща списък от елементите, които са по-големи от двата си съседа
greaterThanNeighbors :: [Int] -> [Int]
greaterThanNeighbors xs = map (\((_,b), _) -> b) $ filter (\((a,b),c) -> b > a && b > c) $ zip (zip xs (drop 1 xs)) (drop 2 xs)

-- Да се дефинира тип за песен. Всяка песен има име, изпълнител, дължина в секунди и брой слушания
data Song = Song {sName :: String, artist :: String, sLength :: Int, listened :: Int}
    deriving Show

-- Да се дефинира тип за плейлист. Плейлиста има име и списък от песни
data Playlist = Playlist {pName :: String, songs :: [Song]}
    deriving Show 

testPlaylist :: Playlist
testPlaylist = Playlist "test1" [Song "test1" "art1" 10 250, Song "test2" "art2" 1 260, Song "test3" "art1" 1 260]

testPlaylist2 :: Playlist
testPlaylist2 = Playlist "test2" [Song "test1" "art0" 10 250, Song "test2" "art2" 1 260, Song "test3" "art1" 1 260]

-- Да се напише функция, която връща името на най-слушаната песен 
mostListened :: Playlist -> String 
mostListened p = sName $ foldl1 (\acc s -> if listened s > listened acc then s else acc) $ songs p

-- Да се напише функция, която връща времето за слушане на цял плейлист в секунди
playlistLength :: Playlist -> Int 
playlistLength p = sum $ map sLength $ songs p 

-- Да се дефинира тип за човек. Всеки човек има име, години и списък от плейлисти
data Person = Person {name :: String, age :: Int, playlists :: [Playlist]}
    deriving Show 

testPerson1 :: Person
testPerson1 = Person "person1" 20 [testPlaylist, testPlaylist2]

testPerson2 :: Person
testPerson2 = Person "person2" 20 [testPlaylist2]

-- Да се дефинира тип за слушатели. 
type Listeners = [Person]

testListeners :: Listeners
testListeners = [testPerson1, testPerson2]

-- Да се напише функция, която приема слушатели и артист. Тя трябва да сортира слушателите по броя пъти, което са слушали артиста. (merge sort)
sortByListens :: Listeners -> String -> Listeners 
sortByListens [] _ = []
sortByListens l a = map snd $ fancyMergeSort zippedList (\(t1, _) (t2, _) -> t1 > t2)
    where
        artistListened :: Playlist -> Int
        artistListened p = sum $ map listened $ filter (\s -> artist s == a) $ songs p
        timesListened :: Person -> Int 
        timesListened p = sum $ map artistListened $ playlists p
        times :: [Int]
        times = map timesListened l
        zippedList :: [(Int, Person)]
        zippedList = zip times l

fancyMergeSort :: [a] -> (a -> a -> Bool) -> [a]
fancyMergeSort [] _ = []
fancyMergeSort [a] _ = [a]
fancyMergeSort as f = merge (fancyMergeSort (take half as) f) (fancyMergeSort (drop half as) f)
    where 
        half = div (length as) 2
        merge [] xs = xs 
        merge xs [] = xs
        merge (x : xs) (y : ys) =  if f x y then x : merge xs (y:ys) else y : merge (x : xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort as = merge (mergeSort (take half as)) (mergeSort (drop half as))
    where 
        half = div (length as) 2
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] xs = xs 
        merge xs [] = xs
        merge (x : xs) (y : ys) =  if x > y then x : merge xs (y:ys) else y : merge (x : xs) ys


-- Да се напише функция, която превръща време в секунди в часове, минути и секунди
-- Да се напише функция, която приема слушатели, артист и име. Тя трябва да върне процента слушатели, които са слушали по-малко от подадения човек.
-- Да се напише функция, която връща средната възраст на хората, които имат подаден артист в поне един от плейлистите си
-- Да се напише функция, която връща имената на всички плейлисти, разделени с подаден символ.
-- Да се напише функция, която приема списък от функции (Person -> Int) и слушатели. Тя прилага всяка функция върху всеки човек и връща двумерен списък от резултатите.