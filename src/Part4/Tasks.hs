{-# LANGUAGE InstanceSigs #-}
module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = reversed REmpty
    where reversed rlst [] = rlst
          reversed rlst (head : tail) = reversed (rlst :< head) tail 

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show lst = "[" <> showNext lst <> "]"
        where
            showNext REmpty = ""
            showNext (REmpty :< head) = show head
            showNext (tail :< head) = showNext tail <> "," <> show head

instance Eq a => Eq (ReverseList a) where
    (==) :: ReverseList a -> ReverseList a -> Bool
    (==) REmpty REmpty = True
    (==) (tailA :< headA) (tailB :< headB) = headA == headB && tailA == tailB
    (==) _ _ = False
    (/=) a b = not (a == b) 
instance Semigroup (ReverseList a) where
instance Monoid (ReverseList a) where
instance Functor ReverseList where
instance Applicative ReverseList where
instance Monad ReverseList where
