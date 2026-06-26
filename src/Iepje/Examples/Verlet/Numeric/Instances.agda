
module Iepje.Examples.Verlet.Numeric.Instances where

open import Iepje.Examples.Verlet.Numeric.Types
open import Iepje.Examples.Verlet.Numeric.Classes

open import Agda.Builtin.Nat as Nat hiding (_+_; _-_; _*_;_<_)
open import Agda.Builtin.Int
open import Agda.Builtin.Float
open import Agda.Builtin.String
open import Agda.Builtin.Maybe
open import Agda.Builtin.Bool

-- Functions with only Nat inputs

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
  hsh-ℕ = mk-sh primShowNat

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

module _ {A : Set} {{_ : Has-* A A A}} (one : A) where

  pow : A → ℕ → A
  pow x 0 = one
  pow x (suc y) = x * pow x y

pow-ℕℕℕ : ℕ → ℕ → ℕ
pow-ℕℕℕ = pow 1
{-# COMPILE JS pow-ℕℕℕ = m => n => (m ** m) #-}

instance
  h^-ℕℕℕ = mk-^ λ x y →                 pow-ℕℕℕ x y
  -- h^-ℕℕℤ = mk-^ λ x y →            pos (pow-ℕℕℕ x y)
  -- h^-ℕℕ𝔽 = mk-^ λ x y → primNatToFloat (pow-ℕℕℕ x y)

-- Functions with only Int (or Nat) inputs

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

instance
  hn-ℤℤ = mk-neg neg-ℤℤ
  hsh-ℤ = mk-sh sh-ℤ

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

  h^-ℤℕℤ = mk-^ λ x y →                 pow (pos 1) x y
  -- h^-ℤℕ𝔽 = mk-^ λ x y → primIntToFloat (pow (pos 1) x y)

  h<-ℤℤ = mk-< λ x y → <-ℤℤ x y
  h<-ℕℤ = mk-< λ x y → <-ℤℤ (pos x) y
  h<-ℤℕ = mk-< λ x y → <-ℤℤ x (pos y)

-- Functions with Float inputs

r-𝔽ℤ : 𝔽 → ℤ
r-𝔽ℤ f with primFloatRound f
... | nothing = pos 0
... | just z = z

instance
  hr-𝔽ℤ = mk-r r-𝔽ℤ
  hsh-𝔽 = mk-sh Agda.Builtin.Float.primShowFloat
  hn-𝔽𝔽 = mk-neg primFloatNegate

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

-- Pair

module _ {A}{{_ : Has-+ A A A}} where
  +-A²A² : A ² → A ² → A ²
  +-A²A² (x₁ , y₁) (x₂ , y₂) = x₁ + x₂ , y₁ + y₂
  instance h+-A²A²  = mk-+ +-A²A²

module _ {A B}{{_ : Has-neg A B}} where
  neg-A² : A ² → B ²
  neg-A² (x , y) = - x , - y
  instance hn-A² = mk-neg neg-A²

module _ {A B C} {{_ : Has-* A B C}} where
  *-A²-l : A → B ² → C ²
  *-A²-l k (x , y) = (k * x) , (k * y)
  *-A²-r : A ² → B → C ²
  *-A²-r (x , y) k = (x * k) , (y * k)
  instance h*-A²-l = mk-* *-A²-l
  instance h*-A²-r = mk-* *-A²-r

module _ {A B C} {{_ : Has-/ A B C}} where
  /-A²-r : A ² → B → C ²
  /-A²-r (x , y) d = (x / d , y / d)
  instance h/-A²-r = mk-/ /-A²-r

module _ {A B} {{_ : Has-round A B}} where
  r-A² : A ² → B ²
  r-A² (x , y) = round x , round y
  instance hr-A² = mk-r r-A²

module _ {A} {{_ : Has-show A String}} where
  sh-A² : A ² → String
  sh-A² (x , y) = "(" ++ show x ++ "," ++ show y ++ ")"
    where _++_ = primStringAppend; infixr 20 _++_
  instance hsh-A² = mk-sh sh-A²

module _ {A B C} {{_ : Has-* A B C}} {{_ : Has-+ C C C}} where
  _∙_ : A ² → B ² → C
  (x₁ , y₁) ∙ (x₂ , y₂) = x₁ * x₂ + y₁ * y₂    

module _ {A B} {{_ : Has-* A A B}} {{_ : Has-+ B B B}} where
  ∣_∣² : A ² → B
  ∣ v ∣² = v ∙ v
