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
myGCD d 0 = abs d
myGCD a b = myGCD b (a `mod` b) 

isLeapYear :: Integer -> Bool
isLeapYear year = (year `mod` 400 == 0) || (year `mod` 100 /= 0 && year `mod` 4 == 0)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect dd mm yyyy = dd > 0 && yyyy > 0 && case mm of
    1  -> dd <= 31
    2  -> if isLeapYear yyyy then dd <= 29 else dd <= 28
    3  -> dd <= 31
    4  -> dd <= 30
    5  -> dd <= 31
    6  -> dd <= 30
    7  -> dd <= 31
    8  -> dd <= 31
    9  -> dd <= 30
    10 -> dd <= 31
    11 -> dd <= 30
    12 -> dd <= 31
    _  -> False

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
