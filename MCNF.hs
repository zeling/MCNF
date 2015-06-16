module Main where

-- | Todo:
-- 1. refactor since most of the code is tedious and redundant.
-- 2. add function distributeModality. I've got no time today.
-- 3. add function checkValidity.
-- 4. add more type restrictions. I guess ModalWff is not good enough, but a better way doesn't come along itself. need help :)
-- 5. add a parser.

import           Control.Applicative

type Variable = String

data ModalWff = L ModalWff
              | M ModalWff
              | Impl ModalWff ModalWff
              | Not ModalWff
              | And ModalWff ModalWff
              | Or ModalWff ModalWff
              | Var Variable
              | Equ ModalWff ModalWff

instance Show ModalWff where
  show = showModalWff

showModalWff :: ModalWff -> String
showModalWff (L wff) = "(L" ++ showModalWff wff ++ ")"
showModalWff (M wff) = "(M" ++ showModalWff wff ++ ")"
showModalWff (Impl awff cwff) = f <$> ($ awff) <*> ($ cwff) $ showModalWff
  where f x y =  "(" ++ x ++ "⊃" ++ y ++ ")"
showModalWff (Not wff) = "(~" ++ showModalWff wff ++ ")"
showModalWff (And wff1 wff2) = f <$> ($ wff1) <*> ($ wff2) $ showModalWff
  where f x y = "(" ++ x ++ "∧" ++ y ++ ")"
showModalWff (Or wff1 wff2) = f <$> ($ wff1) <*> ($ wff2) $ showModalWff
  where f x y = "(" ++ x ++ "∨" ++ y ++ ")"
showModalWff (Var v) = v
showModalWff (Equ wff1 wff2) = f <$> ($ wff1) <*> ($ wff2) $ showModalWff
  where f x y = "(" ++ x ++ "☰" ++ y ++ ")"

-- | Step one : first eliminate all operators except ~, L, M, or, and.
eliminateOperator :: ModalWff -> ModalWff
eliminateOperator (L wff) =  L (eliminateOperator wff)
eliminateOperator (M wff) = M (eliminateOperator wff)
eliminateOperator (Impl awff cwff) = Or (Not ( eliminateOperator awff )) (eliminateOperator cwff)
eliminateOperator (Not wff) = Not (eliminateOperator wff)
eliminateOperator (And wff1 wff2) = And <$> ($ wff1) <*> ($ wff2) $ eliminateOperator
eliminateOperator (Or wff1 wff2) = Or <$> ($ wff1) <*> ($ wff2) $ eliminateOperator
eliminateOperator (Equ wff1 wff2) = And (wff1' `Impl` wff2') (wff2' `Impl` wff1')
  where wff1' = eliminateOperator wff1
        wff2' = eliminateOperator wff2
eliminateOperator wff@_ = wff

-- | Step two : second eliminate every occurrence of ~ immediately before a bracket or a modal operator.
prefixNot :: ModalWff -> ModalWff
prefixNot (Not (L wff)) = M (prefixNot (Not wff))
prefixNot (Not (M wff)) = L (prefixNot (Not wff))
prefixNot (Not (And wff1 wff2)) = Or <$> ($ wff1) <*> ($ wff2) $ (prefixNot . Not)
prefixNot (Not (Or wff1 wff2)) = And <$> ($ wff1) <*> ($ wff2) $ (prefixNot . Not)
prefixNot (And wff1 wff2) = And <$> ($ wff1) <*> ($ wff2) $ prefixNot
prefixNot (Or wff1 wff2) = Or <$> ($ wff1) <*> ($ wff2) $ prefixNot
prefixNot (L wff) = L (prefixNot wff)
prefixNot (M wff) = M (prefixNot wff)
prefixNot wff@_ = wff

hasModalityPrefixed :: ModalWff -> Bool
hasModalityPrefixed (L _) = True
hasModalityPrefixed (M _) = True
hasModalityPrefixed _ = False

stripModality :: ModalWff -> ModalWff
stripModality (L wff) = wff
stripModality (M wff) = wff
stripModality wff@_ = wff

-- | Step 3 : reduce all iterated modalities to single modalities.
-- note : In system S5, iterated modalities can always be reduced to single modalities.
reduceModality :: ModalWff -> ModalWff
reduceModality wff
  | hasModalityPrefixed wff = let strippedWff = stripModality wff in
                                  if hasModalityPrefixed strippedWff
                                  then reduceModality strippedWff
                                  else wff
  | otherwise = wff


distributeModality :: ModalWff -> ModalWff
distributeModality = id -- Just For Now


testValidity :: ModalWff -> Bool
testValidity = undefined -- Just For Now

toMCNF :: ModalWff -> ModalWff
toMCNF = eliminateOperator |> prefixNot |> reduceModality |> distributeModality
  where (|>) = flip (.)


main :: IO ()
main = do
  let wffToBeExamined = Impl (L (And (Var "q") (Var "p"))) (Not (Var "p"))
  print . show . toMCNF $ wffToBeExamined
