
-- Instances with only Int (or Nat) inputs

module Iepje.Examples.Lib.Numeric.Instances.Fin where

open import Iepje.Examples.Lib.Numeric.Types
open import Iepje.Examples.Lib.Numeric.Classes

open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Nat

open import Iepje.Internal.Utils using (map)

==-Fin : ∀{n} → Fin n → Fin n → Bool
==-Fin zero zero = true
==-Fin (suc f1) (suc f2) = ==-Fin f1 f2
==-Fin _ _ = false

instance
  h==-Fin : ∀{n} → Has-== (Fin n) (Fin n) Bool
  h==-Fin = mk-== ==-Fin


enumerate : ∀ n → List (Fin n)
enumerate zero = []
enumerate (suc n) = zero ∷ map suc (enumerate n)
