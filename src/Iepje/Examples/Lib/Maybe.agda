
-- Maybe applicative

module Iepje.Examples.Lib.Maybe where

open import Agda.Builtin.Maybe

private variable
  ℓa ℓb ℓc : Agda.Primitive.Level
  A : Set ℓa
  B : Set ℓb
  C : Set ℓc

_<*>_ : Maybe (A → B) → Maybe A → Maybe B
just f <*> just x = just (f x)
_ <*> _ = nothing

pure : A → Maybe A
pure = just

_<$>_ : (A → B) → Maybe A → Maybe B
f <$> m = pure f <*> m

liftA2 : (A → B → C) → Maybe A → Maybe B → Maybe C
liftA2 f ma mb = (f <$> ma) <*> mb

