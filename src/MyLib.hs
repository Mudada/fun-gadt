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



data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: forall a. Type a -> Type [a]
  RPair :: forall a b. Type a -> Type b -> Type (a, b)
  RDyn :: Type Dynamic
  RFun :: forall a b. Type a -> Type b -> Type (a -> b)

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
