
-- Fast indexing etc. on JS arrays

module Iepje.Examples.Lib.Array where

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool

-- Agda already compiles Lists to JS arrays
Array = List

private variable
  ℓa ℓb : Agda.Primitive.Level
  A : Set ℓa
  B : Set ℓb

take : Nat → Array A → Array A
take zero _ = []
take _ [] = []
take (suc n) (x ∷ l) = x ∷ take n l
{-# COMPILE JS take = _ => _ => n => arr => arr.slice(0, Number(n)) #-}

get-with-default : A → Nat → Array A → A
get-with-default d _ [] = d
get-with-default d zero (a ∷ _) = a
get-with-default d (suc n) (_ ∷ l) = get-with-default d n l
{-# COMPILE JS get-with-default = _ => _ => d => n => arr =>
  (n < BigInt(arr.length)) ? arr[n] : d
#-}

indexed-map : (Nat → A → B) → Array A → Array B
indexed-map {A = A} {B = B} f = go 0 where
  go : Nat → Array A → Array B
  go _ [] = []
  go n (a ∷ as) = f n a ∷ go (suc n) as
{-# COMPILE JS indexed-map = _=> _=> _=> _=> f => as =>
  {
    let bs = new Array(as.length);
    for (let i = 0; i < as.length; i++) {
      bs[i] = f(BigInt(i))(as[i]);
    }
    return bs;
  }
#-}

map : (A → B) → Array A → Array B
map f = indexed-map λ _ → f

set : Nat → A → Array A → Array A
set n a [] = []
set zero a (_ ∷ as) = a ∷ as
set (suc n) a (x ∷ as) = x ∷ set n a as
{-# COMPILE JS set = _=> _ => n => a => arr =>
{
  let copy = arr.slice();
  if (n < BigInt(arr.length))
    {
      copy[n] = a;
    }
  return copy;
}
#-}

filter : (A → Bool) → Array A → Array A
filter _ [] = []
filter f (a ∷ as) with f a
... | true = a ∷ filter f as
... | false = filter f as
{-# COMPILE JS filter = _ => _ => f => arr => arr.filter(f) #-}
