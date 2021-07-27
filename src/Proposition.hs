module Proposition
  ( Proposition (..),
    doubleNegation,
    transform,
    randomProp,
    premises,
  )
where

import BinTree
import Samplers

-- | Propositions of the full propositional calculus.
data Proposition
  = Variable String
  | Implication Proposition Proposition
  | And Proposition Proposition
  | Or Proposition Proposition
  | Negation Proposition
  | FalseProp
  deriving (Eq)

-- | Given p returns ~~p (cf. Glivenko's theorem).
doubleNegation :: Proposition -> Proposition
doubleNegation p = Negation (Negation p)

parens :: ShowS -> ShowS
parens f = ("(" ++) . f . (")" ++)

instance Show Proposition where
  showsPrec _ (Variable x) = (x ++)
  showsPrec n (Implication p q) = parens (p' . (" -> " ++) . q')
    where
      p' = showsPrec n p
      q' = showsPrec n q
  showsPrec n (Or p q) = parens (p' . (" v " ++) . q')
    where
      p' = showsPrec n p
      q' = showsPrec n q
  showsPrec n (And p q) = parens (p' . (" & " ++) . q')
    where
      p' = showsPrec n p
      q' = showsPrec n q
  showsPrec n (Negation p) = ("~" ++) . parens p' where p' = showsPrec n p
  showsPrec _ FalseProp = ("false" ++)

-- | Transforms all ~p sub-propositions into (p -> false).
--   Note: Some solvers do not expand negations automatically.
transform :: Proposition -> Proposition
transform (Implication p q) = Implication p' q'
  where
    p' = transform p
    q' = transform q
transform (Negation p) = Implication p' FalseProp
  where
    p' = transform p
transform x = x

-- | Returns a list of premises of the given proposition.
premises :: Proposition -> [Proposition]
premises (Implication p q) = p : premises q
premises _ = []

randomVariables :: Int -> IO [Proposition]
randomVariables n = do
  xs <- randomPart n
  bs <- randomBools n
  return $ zipWith randomVariable' xs bs

randomVariable' :: String -> Bool -> Proposition
randomVariable' x True = Variable x
randomVariable' x False = Negation (Variable x)

build ::
  [Proposition] ->
  [Bool] ->
  BinTree ->
  ([Proposition], [Bool], Proposition)
build (x : xs) bs Leaf = (xs, bs, x)
build xs (True : bs) (Node l r) =
  (xs'', bs'', Implication l' r')
  where
    (xs', bs', l') = build xs bs l
    (xs'', bs'', r') = build xs' bs' r
build xs (False : bs) (Node l r) =
  (xs'', bs'', Implication l' r')
  where
    (xs', bs', l') = build xs bs l
    (xs'', bs'', r') = build xs' bs' r
build _ _ _ = error "Impossible"

-- | Generates a random proposition of given size.
--   Note: The size of proposition is equal to
--   the number of internal nodes of its corresponding
--   binary tree scaffolding.
randomProp :: Int -> IO Proposition
randomProp n = do
  t <- randomBinTreeIO n
  bs <- randomBools n
  xs <- randomVariables (n + 1)
  let (_, _, p) = build xs bs t
  return p
