
-- Instances for pairs

module Iepje.Examples.Verlet.Numeric.Instances.Pair where

open import Iepje.Examples.Verlet.Numeric.Types
open import Iepje.Examples.Verlet.Numeric.Classes

open import Agda.Builtin.String
open import Agda.Builtin.Bool

module _ {A}{{_ : Has-+ A A A}} where
  +-A²A² : A ² → A ² → A ²
  +-A²A² (x₁ , y₁) (x₂ , y₂) = x₁ + x₂ , y₁ + y₂
  instance h+-A²A²  = mk-+ +-A²A²

module _ {A}{{_ : Has-sub A A A}} where
  s-A²A² : A ² → A ² → A ²
  s-A²A² (x₁ , y₁) (x₂ , y₂) = x₁ - x₂ , y₁ - y₂
  instance hs-A²A²  = mk-s s-A²A²


module _ {A}{{_ : Has-0 A}} where
  0-A² : A ²
  0-A² = get-0 , get-0
  instance h0-A²  = mk-0 0-A²

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
  ∙-A² : A ² → B ² → C
  ∙-A² (x₁ , y₁) (x₂ , y₂) = x₁ * x₂ + y₁ * y₂
  instance h∙-A² = mk-∙ ∙-A²

module _ {A B} {{_ : Has-* A A B}} {{_ : Has-+ B B B}} where
  ∣_∣² : A ² → B
  ∣ v ∣² = v ∙ v

module _ {A B} {{_ : Has-* A A B}} {{_ : Has-+ B B B}} {{_ : Has-^ B 𝔽 B}} where
  ∣_∣ : A ² → B
  ∣ v ∣ = ∣ v ∣² ^ 0.5

module _ {A} {{_ : Has-* A A A}} {{_ : Has-/ A A A}} {{_ : Has-+ A A A}} {{_ : Has-^ A 𝔽 A}} where
  unit : A ² → A ²
  unit v = v / ∣ v ∣
