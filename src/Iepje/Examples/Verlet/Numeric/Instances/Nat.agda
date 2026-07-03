
-- Instances with only Nat inputs

module Iepje.Examples.Verlet.Numeric.Instances.Nat where

open import Iepje.Examples.Verlet.Numeric.Types
open import Iepje.Examples.Verlet.Numeric.Classes

open import Agda.Builtin.Nat as Nat hiding (_+_; _-_; _*_;_<_;_==_)
open import Agda.Builtin.Int
open import Agda.Builtin.Float
open import Agda.Builtin.String


neg-ℕℤ : ℕ → ℤ
neg-ℕℤ zero = pos zero
neg-ℕℤ (suc n) = negsuc n
{-# COMPILE JS neg-ℕℤ = n => (- n) #-}
{-# BUILTIN FROMNEG neg-ℕℤ #-}

sub-ℕℕℤ : ℕ → ℕ → ℤ
sub-ℕℕℤ m zero = pos m
sub-ℕℕℤ zero (suc n) = negsuc n
sub-ℕℕℤ (suc m) (suc n) = sub-ℕℕℤ m n
{-# COMPILE JS sub-ℕℕℤ = x => y => (x - y) #-}

instance
  h0-ℕ = mk-0 0
  h1-ℕ = mk-1 1
  hsh-ℕ = mk-sh primShowNat

  h==-ℕℕ = mk-== Nat._==_
  h*-ℕℕℕ = mk-* Nat._*_
  h+-ℕℕℕ = mk-+ Nat._+_
  -- hs-ℕℕℕ = mk-s Nat._-_ -- actually _∸_

  hs-ℕℕℤ = mk-s sub-ℕℕℤ
  -- h+-ℕℕℤ = mk-+ λ x y → pos (Nat._+_ x y)
  -- h*-ℕℕℤ = mk-* λ x y → pos (Nat._*_ x y)
  hn-ℕℤ  = mk-neg neg-ℕℤ

  h<-ℕℕ = mk-< Nat._<_

  -- hs-ℕℕ𝔽 = mk-s λ x y → primIntToFloat (sub-ℕℕℤ x y)
  -- h+-ℕℕ𝔽 = mk-+ λ x y → primNatToFloat (Nat._+_ x y)
  -- h*-ℕℕ𝔽 = mk-* λ x y → primNatToFloat (Nat._*_ x y)
  -- hn-ℕ𝔽  = mk-neg λ x → primIntToFloat (neg-ℕℤ x)

  h/-ℕℕ𝔽 = mk-/ λ x y → primFloatDiv (primNatToFloat x) (primNatToFloat y)

module _ {A : Set} {{_ : Has-* A A A}} {{_ : Has-1 A}} where

  pow : A → ℕ → A
  pow x 0 = get-1
  pow x (suc y) = x * pow x y

pow-ℕℕℕ : ℕ → ℕ → ℕ
pow-ℕℕℕ = pow
{-# COMPILE JS pow-ℕℕℕ = m => n => (m ** m) #-}

instance
  h^-ℕℕℕ = mk-^ λ x y →                 pow-ℕℕℕ x y
  -- h^-ℕℕℤ = mk-^ λ x y →            pos (pow-ℕℕℕ x y)
  -- h^-ℕℕ𝔽 = mk-^ λ x y → primNatToFloat (pow-ℕℕℕ x y)
