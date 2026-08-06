
-- Identify monad

module Iepje.Examples.Lib.Id where

Id : Set → Set
Id A = A

_>>=_ : ∀{A B} → Id A → (A → Id B) → Id B
a >>= f = f a

pure : ∀{A} → A → Id A
pure a = a
