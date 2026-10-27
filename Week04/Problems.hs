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

-- Oстаналите задачи от миналата седмица
-- За другата седмица си пригответе въпроси

-- Да се дефинира функция, която приема число n и връща първите n реда от триъгълника на Паскал

-- Композиции на функции:
-- Да се напише функция, която приема списък и две функции (а -> a). Тя трябва да приложи двете функции върху всеки елемент
-- Същата задача, но със списък от функции

-- Maybe
-- Да се напише функцията safeHead. 
-- Да се напише функция, която приема списък и предикат. Всеки елемент от списъка, 
-- който не удовлетворява предиката, трябва да се замени с Nothing
-- Да се напише функция, която сумира елементите на списък от Maybe Int

-- Either
-- Да се напише функция, която приема два списъка и дели поелементно. Ако някъде присъства деление на 0 да се върне грешка
-- Да се напише функция splitEithers тя приема списък от Either a b и връща наредена двойка ([a], [b])

