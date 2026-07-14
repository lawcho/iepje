
-- Instances with only Int (or Nat) inputs

module Iepje.Examples.Lib.Numeric.Instances.Int where

open import Iepje.Examples.Lib.Numeric.Types
open import Iepje.Examples.Lib.Numeric.Classes

open import Agda.Builtin.Nat as Nat hiding (_+_; _-_; _*_;_<_;_==_)
open import Agda.Builtin.Int
open import Agda.Builtin.Float
open import Agda.Builtin.String
open import Agda.Builtin.Bool

open import Iepje.Examples.Lib.Numeric.Instances.Nat

abs : ℤ → ℕ
abs (pos n) = n
abs (negsuc n) = suc n

neg-ℤℤ : ℤ → ℤ
neg-ℤℤ = λ where
  (pos zero)    → pos zero
  (pos (suc n)) → negsuc n
  (negsuc n)    → pos (suc n)
{-# COMPILE JS neg-ℤℤ = n => (- n) #-}

sh-ℤ : ℤ → String
sh-ℤ (pos n) = primShowNat n
sh-ℤ (negsuc n) = primStringAppend "-" (primShowNat (suc n))

+-ℤℤℤ : ℤ → ℤ → ℤ
+-ℤℤℤ (pos m) (pos n)       = pos (m + n)
+-ℤℤℤ (negsuc m) (negsuc n) = negsuc (suc (m + n))
+-ℤℤℤ (pos m) (negsuc n) = sub-ℕℕℤ m (suc n)
+-ℤℤℤ (negsuc m) (pos n) = sub-ℕℕℤ n (suc m)
{-# COMPILE JS +-ℤℤℤ = x => y => (x + y) #-}

s-ℤℤℤ : ℤ → ℤ → ℤ
s-ℤℤℤ x y = +-ℤℤℤ x (neg-ℤℤ y) 
{-# COMPILE JS s-ℤℤℤ = x => y => (x - y) #-}

*-ℤℤℤ : ℤ → ℤ → ℤ
*-ℤℤℤ (pos m) (pos n)       = pos (m * n)
*-ℤℤℤ (negsuc m) (negsuc n) = pos (suc m * suc n)
*-ℤℤℤ (pos m) (negsuc n)    = neg-ℕℤ (m * suc n)
*-ℤℤℤ (negsuc m) (pos n)    = neg-ℕℤ (suc m * n)
{-# COMPILE JS *-ℤℤℤ = x => y => (x * y) #-}

<-ℤℤ : ℤ → ℤ → Bool
<-ℤℤ (pos m) (pos n) = m Nat.< n
<-ℤℤ (pos _) (negsuc _) = false
<-ℤℤ (negsuc _) (pos _) = true
<-ℤℤ (negsuc m) (negsuc n) = n Nat.< m
{-# COMPILE JS <-ℤℤ = x => y => (x < y) #-}

==-ℤℤ : ℤ → ℤ → Bool
==-ℤℤ (pos m) (pos n) = m == n
==-ℤℤ (negsuc m) (negsuc n) = m == n
==-ℤℤ _ _  = false
{-# COMPILE JS ==-ℤℤ = x => y => (x == y) #-}

instance
  h0-ℤ = mk-0 (pos 0)
  h1-ℤ = mk-0 (pos 1)
  hn-ℤℤ = mk-neg neg-ℤℤ
  hsh-ℤ = mk-sh sh-ℤ

  h==-ℤℤ = mk-== ==-ℤℤ
  h+-ℤℤℤ = mk-+ λ x y → +-ℤℤℤ x y
  h+-ℤℕℤ = mk-+ λ x y → +-ℤℤℤ x (pos y)
  h+-ℕℤℤ = mk-+ λ x y → +-ℤℤℤ (pos x) y

  -- h+-ℤℤ𝔽 = mk-+ λ x y → primIntToFloat (+-ℤℤℤ x y)
  -- h+-ℤℕ𝔽 = mk-+ λ x y → primIntToFloat (+-ℤℤℤ x (pos y))
  -- h+-ℕℤ𝔽 = mk-+ λ x y → primIntToFloat (+-ℤℤℤ (pos x) y)

  hs-ℤℤℤ = mk-s λ x y → s-ℤℤℤ x y
  hs-ℤℕℤ = mk-s λ x y → s-ℤℤℤ x (pos y)
  hs-ℕℤℤ = mk-s λ x y → s-ℤℤℤ (pos x) y

  -- hs-ℤℤ𝔽 = mk-s λ x y → primIntToFloat (s-ℤℤℤ x y)
  -- hs-ℤℕ𝔽 = mk-s λ x y → primIntToFloat (s-ℤℤℤ x (pos y))
  -- hs-ℕℤ𝔽 = mk-s λ x y → primIntToFloat (s-ℤℤℤ (pos x) y)

  h*-ℤℤℤ = mk-* λ x y → *-ℤℤℤ x y
  -- h*-ℤℕℤ = mk-* λ x y → *-ℤℤℤ x (pos y)
  -- h*-ℕℤℤ = mk-* λ x y → *-ℤℤℤ (pos x) y

  -- h*-ℤℤ𝔽 = mk-* λ x y → primIntToFloat (*-ℤℤℤ x y)
  -- h*-ℤℕ𝔽 = mk-* λ x y → primIntToFloat (*-ℤℤℤ x (pos y))
  -- h*-ℕℤ𝔽 = mk-* λ x y → primIntToFloat (*-ℤℤℤ (pos x) y)

  h/-ℤℤ𝔽 = mk-/ λ x y → primFloatDiv (primIntToFloat x) (primIntToFloat y)
  h/-ℕℤ𝔽 = mk-/ λ x y → primFloatDiv (primNatToFloat x) (primIntToFloat y)
  h/-ℤℕ𝔽 = mk-/ λ x y → primFloatDiv (primIntToFloat x) (primNatToFloat y)

  h^-ℤℕℤ = mk-^ λ x y →                 pow x y
  -- h^-ℤℕ𝔽 = mk-^ λ x y → primIntToFloat (pow (pos 1) x y)

  h<-ℤℤ = mk-< λ x y → <-ℤℤ x y
  h<-ℕℤ = mk-< λ x y → <-ℤℤ (pos x) y
  h<-ℤℕ = mk-< λ x y → <-ℤℤ x (pos y)
