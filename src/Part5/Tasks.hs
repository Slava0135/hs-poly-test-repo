{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (head : tail) = myFoldl f (f acc head) tail

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (head : tail) = f head (myFoldr f acc tail)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldl (\acc x -> acc <> [f x]) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldl (\acc x -> acc <> f x) []

myConcat :: [[a]] -> [a]
myConcat = notImplementedYet

myReverse :: [a] -> [a]
myReverse = notImplementedYet

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = notImplementedYet

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = notImplementedYet

