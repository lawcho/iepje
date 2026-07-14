
-- Instances with Float inputs

module Iepje.Examples.Lib.Numeric.Instances.Float where

open import Iepje.Examples.Lib.Numeric.Types
open import Iepje.Examples.Lib.Numeric.Classes

open import Agda.Builtin.Nat as Nat hiding (_+_; _-_; _*_;_<_)
open import Agda.Builtin.Int
open import Agda.Builtin.Float
open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Agda.Builtin.Maybe

open import Iepje.Examples.Lib.Numeric.Instances.Nat
open import Iepje.Examples.Lib.Numeric.Instances.Int

r-𝔽ℤ : 𝔽 → ℤ
r-𝔽ℤ f with primFloatRound f
... | nothing = pos 0
... | just z = z

instance
  h0-𝔽 = mk-0 0.0
  h1-𝔽 = mk-1 1.1
  hr-𝔽ℤ = mk-r r-𝔽ℤ
  hsh-𝔽 = mk-sh Agda.Builtin.Float.primShowFloat
  hn-𝔽𝔽 = mk-neg primFloatNegate

  h==-𝔽𝔽 = mk-==        primFloatEquality
  h+-𝔽𝔽𝔽 = mk-+         primFloatPlus
  h+-𝔽ℕ𝔽 = mk-+ λ f n → primFloatPlus f (primNatToFloat n)
  h+-ℕ𝔽𝔽 = mk-+ λ n f → primFloatPlus (primNatToFloat n) f
  h+-𝔽ℤ𝔽 = mk-+ λ f i → primFloatPlus f (primIntToFloat i)
  h+-ℤ𝔽𝔽 = mk-+ λ i f → primFloatPlus (primIntToFloat f) i

  hs-𝔽𝔽𝔽 = mk-s         primFloatMinus
  hs-𝔽ℕ𝔽 = mk-s λ f n → primFloatMinus f (primNatToFloat n)
  hs-ℕ𝔽𝔽 = mk-s λ n f → primFloatMinus (primNatToFloat n) f
  hs-𝔽ℤ𝔽 = mk-s λ f i → primFloatMinus f (primIntToFloat i)
  hs-ℤ𝔽𝔽 = mk-s λ i f → primFloatMinus (primIntToFloat f) i

  h*-𝔽𝔽𝔽 = mk-*         primFloatTimes
  h*-𝔽ℕ𝔽 = mk-* λ f n → primFloatTimes f (primNatToFloat n)
  h*-ℕ𝔽𝔽 = mk-* λ n f → primFloatTimes (primNatToFloat n) f
  h*-𝔽ℤ𝔽 = mk-* λ f i → primFloatTimes f (primIntToFloat i)
  h*-ℤ𝔽𝔽 = mk-* λ i f → primFloatTimes (primIntToFloat f) i

  h/-𝔽𝔽𝔽 = mk-/         primFloatDiv
  h/-𝔽ℕ𝔽 = mk-/ λ f n → primFloatDiv f (primNatToFloat n)
  h/-ℕ𝔽𝔽 = mk-/ λ n f → primFloatDiv (primNatToFloat n) f
  h/-𝔽ℤ𝔽 = mk-/ λ f i → primFloatDiv f (primIntToFloat i)
  h/-ℤ𝔽𝔽 = mk-/ λ i f → primFloatDiv (primIntToFloat f) i

  h^-𝔽𝔽𝔽 = mk-^         primFloatPow
  h^-𝔽ℕ𝔽 = mk-^ λ f n → primFloatPow f (primNatToFloat n)
  h^-ℕ𝔽𝔽 = mk-^ λ n f → primFloatPow (primNatToFloat n) f
  h^-𝔽ℤ𝔽 = mk-^ λ f i → primFloatPow f (primIntToFloat i)
  h^-ℤ𝔽𝔽 = mk-^ λ i f → primFloatPow (primIntToFloat f) i

  h<-𝔽𝔽𝔽 = mk-<         primFloatLess
  h<-𝔽ℕ𝔽 = mk-< λ f n → primFloatLess f (primNatToFloat n)
  h<-ℕ𝔽𝔽 = mk-< λ n f → primFloatLess (primNatToFloat n) f
  h<-𝔽ℤ𝔽 = mk-< λ f i → primFloatLess f (primIntToFloat i)
  h<-ℤ𝔽𝔽 = mk-< λ i f → primFloatLess (primIntToFloat f) i
