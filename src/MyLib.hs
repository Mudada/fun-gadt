{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module MyLib () where

data Term a where
  Zero :: Term Int
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
