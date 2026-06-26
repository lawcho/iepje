
module Iepje.Examples.Verlet.Numeric.Types where

open import Agda.Builtin.Nat
open import Agda.Builtin.Int
open import Agda.Builtin.Float

ℕ = Nat
ℤ = Int
𝔽 = Float

record _² (A : Set) : Set where
  constructor _,_
  field x : A
  field y : A

