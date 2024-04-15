{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

module MyLib () where

import Control.Monad (liftM)
import GHC.Show (appPrec1)

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
  RDyn :: forall a. Type Dynamic

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

tequal :: forall t u. Type t -> Type u -> Maybe(t -> u)
tequal RInt RInt = return id
tequal RChar RChar = return id
tequal (RList ra) (RList rb) = fmap <$> tequal ra rb
-- TODO: RPair
