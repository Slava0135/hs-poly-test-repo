module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

normalizeAngle :: Double -> Double
normalizeAngle x = x - 2*pi * fromIntegral (round (x / (2*pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinN (normalizeAngle x) 0

sinN :: Double -> Integer -> Double
sinN x 0 = x + sinN x 1
sinN x 9 = 0
sinN x n =
    let sign = if even n then 1.0 else -1.0
        nTwo = 2*n + 1
    in (sign * (x ^ nTwo) / fromInteger (factorial nTwo)) + sinN x (n + 1)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = mySin (x + pi/2)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD = notImplementedYet

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect = notImplementedYet

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow = notImplementedYet

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = notImplementedYet

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
