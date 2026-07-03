
-- Instances for vectors

module Iepje.Examples.Verlet.Numeric.Instances.Vector where

open import Iepje.Examples.Verlet.Numeric.Types
open import Iepje.Examples.Verlet.Numeric.Classes
open import Iepje.Examples.Verlet.Numeric.Instances.Applicative
open import Iepje.Examples.Verlet.Numeric.Instances.Fin

open import Agda.Builtin.Nat as Nat using (Nat; zero; suc)
open import Agda.Builtin.Equality
open import Agda.Builtin.Bool
open import Iepje.Internal.Utils using (_&&_)

instance
  hpure-FVec : ∀{n A} → Has-pure A (FVec A n)
  hpure-FVec = mk-pure λ a _ → a

  h<*>-FVec : ∀{n A B} → Has-<*> (FVec (A → B) n) (FVec A n) (FVec B n)
  h<*>-FVec = mk-<*> λ v1 v2 i → v1 i (v2 i)

  h0-FVec : ∀{n A} → {{_ : Has-0 A}} → Has-0 (FVec A n)
  h0-FVec = mk-0 λ _ → get-0

module VecOps where

  replicate : ∀{n A} → A → Vec A n
  replicate {n = zero} a = []
  replicate {n = suc n} a = a ∷ replicate a

  zip : ∀{n A B} → Vec (A → B) n → Vec A n → Vec B n
  zip [] [] = []
  zip (f ∷ fs) (a ∷ as) = f a ∷ zip fs as

  sum : ∀{n A B} → {{_ : Has-+ A B B}} {{_ : Has-0 B}} → Vec A n → B
  sum [] = get-0
  sum (x ∷ v) = x + sum v

module Conversion where

  tabulate : ∀{n A} → FVec A n → Vec A n
  tabulate {zero} f = []
  tabulate {suc n} f = f zero ∷ tabulate λ m → f (suc m)

  lookup : ∀{n A} → Vec A n → FVec A n
  lookup (x ∷ _) zero = x
  lookup (_ ∷ v) (suc m) = lookup v m

instance
  hpure-Vec : ∀{n A} → Has-pure A (Vec A n)
  hpure-Vec = mk-pure VecOps.replicate

  h<*>-Vec : ∀{n A B} → Has-<*> (Vec (A → B) n) (Vec A n) (Vec B n)
  h<*>-Vec = mk-<*> VecOps.zip

  h0-Vec : ∀{n A} → {{_ : Has-0 A}} → Has-0 (Vec A n)
  h0-Vec = mk-0 (VecOps.replicate get-0)

  h*-Vec-l : ∀{n A B C} → {{_ : Has-* A B C}} → Has-* A (Vec B n) (Vec C n)
  h*-Vec-l = mk-* λ a vb → pure (a *_) <*> vb

  h*-Vec-r : ∀{n A B C} → {{_ : Has-* A B C}} → Has-* (Vec A n) B (Vec C n)
  h*-Vec-r = mk-* λ va b → pure (_* b) <*> va

module Vec-Applicative-Instances {n} = Applicative-Instances (λ A → Vec A n)
module FVec-Applicative-Instances {n} = Applicative-Instances (λ A → FVec A n)
