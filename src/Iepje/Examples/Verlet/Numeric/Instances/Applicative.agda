
-- Instances for arbitrary applicatives

module Iepje.Examples.Verlet.Numeric.Instances.Applicative where

open import Iepje.Examples.Verlet.Numeric.Classes

module Applicative-Instances (f : Set → Set) {{_ : ∀{a} → Has-pure a (f a)}} {{_ : ∀{a b} → Has-<*> (f (a → b)) (f a) (f b)}} where

  module _ {A B C} {{_ : Has-+ A B C}} where
    +-Applicative : f A → f B → f C
    +-Applicative fa fb = pure _+_ <*> fa <*> fb
    instance h+-Applicative = mk-+ +-Applicative

  module _ {A B C} {{_ : Has-* A B C}} where
    *-Applicative : f A → f B → f C
    *-Applicative fa fb = pure _*_ <*> fa <*> fb
    instance h*-Applicative = mk-* *-Applicative

  module _ {A B C} {{_ : Has-sub A B C}} where
    s-Applicative : f A → f B → f C
    s-Applicative fa fb = pure _-_ <*> fa <*> fb
    instance hs-Applicative = mk-s s-Applicative

  module _ {A B C} {{_ : Has-^ A B C}} where
    ^-Applicative : f A → f B → f C
    ^-Applicative fa fb = pure _^_ <*> fa <*> fb
    instance h^-Applicative = mk-^ ^-Applicative

  module _ {A B C} {{_ : Has-/ A B C}} where
    /-Applicative : f A → f B → f C
    /-Applicative fa fb = pure _/_ <*> fa <*> fb
    instance h/-Applicative = mk-/ /-Applicative

  module _ {A B C} {{_ : Has-< A B C}} where
    <-Applicative : f A → f B → f C
    <-Applicative fa fb = pure _<_ <*> fa <*> fb
    instance h<-Applicative = mk-< <-Applicative

  module _ {A B} {{_ : Has-neg A B}} where
    neg-Applicative : f A → f B
    neg-Applicative fa = pure neg <*> fa
    instance hn-Applicative = mk-neg neg-Applicative
