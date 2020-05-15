{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Type.Equality
import Data.Type.Bool

data A
data B
data C

data Sort a where
  C1 :: Sort A
  C2 :: Sort B
  C3 :: Sort C
  L  :: [a] ->  Sort a


foo :: (a == A || a == C) ~ 'True => Sort a -> Int
foo C1 = 1
foo C3 = 2
foo (L _) = 3

