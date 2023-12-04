{-# LANGUAGE FlexibleInstances #-}

module Part6.Tasks where

import Data.Map
import Data.Sequence (mapWithIndex)
import Util (notImplementedYet)
import qualified Data.Maybe

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
  zeroMatrix :: Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  (@) m (0, 0) = Just m
  (@) m (_, _) = Nothing
  eyeMatrix 1 = 1
  zeroMatrix 1 1 = 0

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first, rest) = Prelude.splitAt n list

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

  eyeMatrix w =
    let flat = Prelude.map (\it -> if it `rem` (w + 1) == 0 then 1 else 0) [0 .. w * w - 1]
        unflatten = splitEvery w flat
     in unflatten
  zeroMatrix w h = [[]]

instance Matrix (SparseMatrix Int) where
  (@) m (col, row) = case (0 < col && col < sparseMatrixWidth m, 0 < row && row < sparseMatrixHeight m) of
       (True, True) -> Just $ Data.Maybe.fromMaybe 0 (sparseMatrixElements m !? (col, row))
       _ -> Nothing
  eyeMatrix w = SparseMatrix w w (Data.Map.fromList $ Prelude.map (\idx -> ((idx, idx), 1)) [0 .. (w - 1)])
  zeroMatrix w h = SparseMatrix w h empty

-- Реализуйте следующие функции
-- Единичная матрица
eye :: (Matrix m) => Int -> m
eye = eyeMatrix

-- Матрица, заполненная нулями
zero :: (Matrix m) => Int -> Int -> m
zero = zeroMatrix

-- Перемножение матриц
multiplyMatrix :: (Matrix m) => m -> m -> m
multiplyMatrix = notImplementedYet

-- Определитель матрицы
determinant :: (Matrix m) => m -> Int
determinant = notImplementedYet
