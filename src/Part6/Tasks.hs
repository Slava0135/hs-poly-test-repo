{-# LANGUAGE FlexibleInstances #-}

module Part6.Tasks where

import Data.Map
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  (@) :: mx -> (Int, Int) -> Maybe Int
  eyeMatrix :: Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  (@) m (0, 0) = Just m
  (@) m (_, _) = Nothing
  eyeMatrix 1 = 1

instance Matrix [[Int]] where
  (@) m (col, row) =
    if 0 < col && col < length m
      then
        ( let sublist = m !! col
           in if 0 < row && row < length sublist
                then Just (sublist !! col)
                else Nothing
        )
      else Nothing
  eyeMatrix w = [[]]

instance Matrix (SparseMatrix Int) where
  (@) m (col, row) = sparseMatrixElements m !? (col, row)
  eyeMatrix w = SparseMatrix w w empty

-- Реализуйте следующие функции
-- Единичная матрица
eye :: (Matrix m) => Int -> m
eye = eyeMatrix

-- Матрица, заполненная нулями
zero :: (Matrix m) => Int -> Int -> m
zero w h = notImplementedYet

-- Перемножение матриц
multiplyMatrix :: (Matrix m) => m -> m -> m
multiplyMatrix = notImplementedYet

-- Определитель матрицы
determinant :: (Matrix m) => m -> Int
determinant = notImplementedYet
