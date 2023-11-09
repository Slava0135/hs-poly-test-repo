module Part3.Tasks where

import Util (notImplementedYet)
import Data.Ord (comparing)
import Data.List (maximumBy)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums =
    let allDigits = concatMap show nums
        digits = "0123456789"
        countDigit d = (d, length (filter (d==) allDigits))
        timesEach = map countDigit digits
        mostFreqDigit = fst $ maximumBy (comparing snd) timesEach
    in read [mostFreqDigit]

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst = nextUniq lst []
nextUniq [] unq = unq
nextUniq lst unq =
    let next = head lst
    in nextUniq (filter (next/=) lst) (unq ++ [next])

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l =
    let possibleResults = uniq $ map f l
        sameResult r = filter (\x -> f x == r) l
    in map (\r -> (r, sameResult r)) possibleResults
