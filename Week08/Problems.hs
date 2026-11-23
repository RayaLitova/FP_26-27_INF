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

-- Отучвания

-- Да се дефинира тип за двоично дърво
-- Да се дефинират fmap, apply и bind за двоичното дърво.
-- Да се дефинира функция, която приема две дървета и функция. Тя трябва да приложи функцията върху всички елементи на дървото, които са на еднакви позиции
-- Да се дефинира функция, която приема BTree Int и връща BTree Char като всяко число се заменя с ASCII символа, на който съответства.
-- Да се дефинира функцията fold за двоично дърво
-- Да се дефинира функция validate, която проверява дали всеки елемент на дървото е уникален
-- Да се дефинира функция, която приема стринг от вида 1 (2 () 3 (4 ())). Тя трябва да превръща стринга в BTree Int
-- Да се дефинира функция, която чете от терминала стринг във формата от предната задача. Тя трябва да построи дървото и да го валидира.