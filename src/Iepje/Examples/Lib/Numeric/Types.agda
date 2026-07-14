
module Iepje.Examples.Lib.Numeric.Types where

open import Agda.Builtin.Nat
open import Agda.Builtin.Int
open import Agda.Builtin.Float
open import Agda.Builtin.Equality
open import Agda.Builtin.List
open import Agda.Builtin.Bool

ℕ = Nat
ℤ = Int
𝔽 = Float

record _² (A : Set) : Set where
  constructor _,_
  field x : A
  field y : A

data Vec (A : Set) : Nat → Set where
  [] : Vec A 0
  _∷_ : ∀{n} → A → Vec A n → Vec A (suc n)

data Fin : Nat → Set where
  zero : ∀{n} → Fin (suc n)
  suc : ∀{n} → Fin n → Fin (suc n)

FVec : Set → Nat → Set
FVec A n = Fin n → A
