
-- Agda interface to JavaScript sub-classing

module Iepje.Internal.JS.Language.SubTyping where

open import Agda.Builtin.Equality

-- (A extends B) iff, after compilation to JavaScript,
-- A's direct super-class is B
postulate _extends_ : Set → Set → Set

-- Helper definiton for 'up'
data _extends*_ (A : Set) : Set → Set₁ where instance
  extends*-refl : A extends* A
  extends*-cons : ∀{B C}
    → {{A extends  B}}
    → {{B extends* C}}
    → A extends* C
{-# OVERLAPS extends*-refl extends*-cons #-}

-- The (right-nested) definition of _extends*_ helps instance search
-- construct (A extends* B) values starting from A, e.g.
--
--  (known type of `a`)                             (required type of `up a`)
--  HTMLInputElement                                 Node
--  HTMLInputElement --> HTMLElement                 Node
--  HTMLInputElement --> HTMLElement --> Element     Node
--  HTMLInputElement --> HTMLElement --> Element --> Node
--
-- searching up from A never requires backtracking,
-- since every JS class has at most 1 direct super-class

-- When (A extends* B), a value of A may be used wherever a B is expected
-- (aka the "Liskov substitution principle")
postulate up : ∀{A B : Set} → @0 {{A extends* B}} → A → B
{-# COMPILE JS up = _ => _ => _ => a => a #-}
-- Without @0, the (A extends* B) argument would remain until JS run-time,
-- then be strictly evaluated, and crash for postulated extends*

-- Misc. helper for getting a value from instance search
it : ∀{ℓ}{A : Set ℓ} → {{A}} → A
it {{a}} = a

∘-extends* : ∀{A B C} → A extends* B → B extends* C → A extends* C
∘-extends* extends*-refl B<C = B<C
∘-extends* (extends*-cons {{A<B1}} {{B1<B}}) B<C = extends*-cons {{A<B1}} {{∘-extends* B1<B B<C}}
