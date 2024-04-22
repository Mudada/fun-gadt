{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

module MyLib () where

import Control.Monad (liftM2)
import Control.Arrow ((***))

data Term a where
  Zero :: Term Int
  One :: Term Int
  Two :: Term Int
  Three :: Term Int
  Succ, Pred :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: forall a. Term Bool -> Term a -> Term a -> Term a

eval :: forall t. Term t -> t
eval Zero = 0
eval (Succ e) = eval e + 1
eval (Pred e) = eval e - 1
eval (IsZero e) = eval e == 0
eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3

-- >>> eval (If (IsZero (Pred Zero)) (Pred Zero) (Succ Zero))
-- 1

type Name = String
type Age = Int
data Person = Person Name Age
  deriving (Show)

data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: forall a. Type a -> Type [a]
  RPair :: forall a b. Type a -> Type b -> Type (a, b)
  RDyn :: Type Dynamic
  RFun :: forall a b. Type a -> Type b -> Type (a -> b)
  RPerson :: Type Person

deriving instance Show (Type t)

rString :: Type String
rString = RList RChar

data Dynamic = forall t. Dyn (Type t) t

equality :: forall t. Type t -> t -> t -> Bool
equality RInt a b = a == b
equality RChar a b = a == b
equality (RPair ra rb) (a, b) (a', b') = equality ra a a' && equality rb b b'
equality (RList _) [] [] = True
equality (RList _) (_: _) [] = False
equality (RList _) [] (_: _) = False
equality (RList ra) (a:as) (b:bs) = equality ra a b && equality (RList ra) as bs

compare' :: forall t. Type t -> t -> t -> Ordering
compare' RInt a b | a == b = EQ
                 | a < b = LT 
                 | otherwise = GT
compare' RChar a b | a == b = EQ
                  | a < b = LT
                  | otherwise = GT
compare' (RPair ra rb) (a, b) (a', b') = compare (compare' ra a a') (compare' rb b b')
compare' (RList _) [] [] = EQ
compare' (RList _) _ [] = GT
compare' (RList _) [] _ = LT
compare' (RList ra) (a:as) (b:bs) = if comp == EQ then compare' (RList ra) as bs else comp 
  where comp = compare' ra a b

rPair :: forall a b a1 b1. (a -> a1) -> (b -> b1) -> ((a, b) -> (a1, b1))
rPair f g (a, b) = (f a, g b)

rFunc :: forall a b a1 b1. (a -> a1) -> (b -> b1) -> ((a -> b) -> (a1 -> b1))
rFunc f g h = undefined

tequal :: forall t u. Type t -> Type u -> Maybe(t -> u)
tequal RInt RInt = return id
tequal RChar RChar = return id
tequal (RList ra) (RList rb) = fmap <$> tequal ra rb
tequal (RPair a b) (RPair a' b') = (***) <$> tequal a a' <*> tequal b b'
tequal (RFun a b) (RFun a' b') = (\convA convB f -> convB . f . convA) <$> tequal a' a <*> tequal b b'
tequal _ _ = fail "cannot equal"

cast :: forall t. Dynamic -> Type t -> Maybe t
cast (Dyn ra a) t = fmap (\f -> f a) (tequal ra t) 

type Traversal = forall t. Type t -> t -> t

copy :: Traversal
copy (RInt) i = id i

tick :: Name -> Traversal
tick name RPerson (Person n a) | name == n = Person n (a + 1)
tick _ _ t = t

infixl 9 ◦
(◦) :: Traversal -> Traversal -> Traversal
(f ◦ g) rt = f rt . g rt

-- ( Type t -> t -> t ) -> ( Type t -> t -> t ) 
imap :: Traversal -> Traversal
imap _ RInt i = i
imap _ RChar c = c
imap _ (RList _) [] = []
imap f (RList ra) (a:as) = f ra a : f (RList ra) as
imap f (RPair ra rb) (a, b) = (f ra a, f rb b)
imap f RPerson (Person n a) = Person (f rString n) (f RInt a)

-- Main> let ps = [Person "Norma" 50,Person "Richard" 59]
-- Main> everywhere (tick "Richard") (RList RPerson) ps 
-- [Person "Norma" 50,Person "Richard" 60]

everywhere, everywhere' :: Traversal -> Traversal
everywhere f = f ◦ imap (everywhere f)
everywhere' f = imap (everywhere' f) ◦ f

