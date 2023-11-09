{-# LANGUAGE InstanceSigs #-}
module Part4.Tasks where

import Util(notImplementedYet)
import Control.Applicative

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
  (<>) a REmpty = a
  (<>) a (tailB :< headB) = a <> tailB :< headB

instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap f REmpty = REmpty
  fmap f (tail :< head) = fmap f tail :< f head

instance Applicative ReverseList where
  pure x = REmpty :< x
  liftA2 f (tailA :< headA) b = liftA2 f tailA b <> fmap (f headA) b
  liftA2 f _ _ = REmpty


instance Monad ReverseList where
  (>>=) REmpty _ = REmpty
  (>>=) (tail :< head) f = (>>=) tail f <> f head
