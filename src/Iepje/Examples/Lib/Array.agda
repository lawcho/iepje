
-- Fast indexing etc. on JS arrays

module Iepje.Examples.Lib.Array where

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.Float
open import Iepje.Examples.Lib.Float as Float hiding (max; min)

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

length : List A → Nat
length [] = 0
length (_ ∷ l) = 1 + length l
{-# COMPILE JS length = _ => _ => arr => BigInt (arr.length) #-}

-- Postulated to avoid TC-time/run-time result mismatch due to FP errors
postulate sum : List Float → Float
-- sum [] = 0.0
-- sum (x ∷ l) = primFloatPlus x (sum l)
{-# COMPILE JS sum = arr => Math.sumPrecise(arr) #-}

max : List Float → Float
max [] = -Infinity
max (x ∷ l) = Float.max x (max l)
{-# COMPILE JS max = arr => Math.max(...arr) #-}

min : List Float → Float
min [] = +Infinity
min (x ∷ l) = Float.min x (min l)
{-# COMPILE JS min = arr => Math.min(...arr) #-}

mean : List Float → Float
mean l = primFloatDiv (sum l) (primNatToFloat (length l))

fori : List A → (Nat → A → B) → List B
fori l f = indexed-map f l

foldl : (B → A → B) → B → Array A → B
foldl f b [] = b
foldl f b (a ∷ as) = foldl f (f b a) as
{-# COMPILE JS foldl = _ => _ => _ => _ => f => b => arr =>
  arr.reduce
    ( (accumulator,currentValue) => f(accumulator)(currentValue)
    , b
    )
#-}

foldr : (A → B → B) → B → Array A → B
foldr f b [] = b
foldr f b (a ∷ as) = f a (foldr f b as)
{-# COMPILE JS foldr = _ => _ => _ => _ => f => b => arr =>
  arr.reduceRight
    ( (accumulator,currentValue) => f(currentValue)(accumulator)
    , b
    )
#-}

concat : Array A → Array A → Array A
concat [] l2 = l2
concat (a ∷ l1) l2 = a ∷ (concat l1 l2)
{-# COMPILE JS concat = _ => _ => arr1 => arr2 => arr1.concat(arr2) #-}

snoc : List A → A → List A
snoc l a = concat l (a ∷ [])

reverse : Array A → Array A
reverse [] = []
reverse (a ∷ as) = snoc (reverse as) a
{-# COMPILE JS reverse = _ => _ => arr => arr.toReversed() #-}