
-- A module to dump misc. helper functions like `map`

module Iepje.Internal.Utils where

open import Iepje.Internal.JS.Language.IO

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Bool
open import Agda.Builtin.String
open import Agda.Builtin.Sigma
open import Agda.Builtin.Unit
open import Agda.Primitive

private variable
  ℓa ℓb ℓc : Agda.Primitive.Level
  A : Set ℓa
  B : Set ℓb
  C : Set ℓc
  B' : A → Set ℓb

-- Lists

map : (A → B) → List A → List B
map _ [] = []
map f (x ∷ xs) = f x ∷ map f xs

for :  List A → (A → B) → List B
for l f = map f l

foldr : (A → B → B) → B → List A → B
foldr f z = go where
  go : _ → _
  go []         = z
  go (y ∷ ys) = f y (go ys)

-- Booleans

not : Bool → Bool
not false = true
not true  = false

_&&_ : Bool → Bool → Bool
true && true = true
_    && _    = false
infixl 3.5 _&&_

_||_ : Bool → Bool → Bool
false || false = false
_     || _      = true
infixl 3 _||_

-- Nats

_/_ : Nat → Nat → Nat
n / m = div-helper 0 (m - 1) n (m - 1)
infixl 22 _/_

_%_ : Nat → Nat → Nat
n % m = mod-helper 0 (m - 1) n (m - 1)
infixl 22 _%_

-- Control flow

_$_ : ((a : A) → B' a) → (a : A) → B' a
f $ x = f x
infixr 1 _$_

_&_ : (a : A) → ((a : A) → B' a) → B' a
x & f = f x

_∘_ : (B → C) → (A → B) → (A → C)
(g ∘ f) x = g (f x)

case_of_ : A → (A → B) → B
case x of f = f x

if_then_else_ : Bool → A → A → A
if true  then t else _ = t
if false then _ else e = e
infixr 20 if_then_else_

-- Strings

_++_ = primStringAppend
infixl 20 _++_

-- IO

_<$>_ : (A → B) → IO A → IO B
f <$> ma = do a ← ma; pure (f a)
infixr 21 _<$>_

_<&>_ : IO A → (A → B) → IO B
x <&> f = f <$> x

_>>_ : IO A → IO B → IO B
ma >> mb = do _ ← ma; mb

_<<_ : IO A → IO B → IO A
ma << mb = mb >> ma

_<*>_ : IO (A → B) → IO A → IO B
mf <*> mx = do f ← mf; x ← mx; pure (f x)
infixl 20 _<*>_

_=<<_ : (A → IO B) → IO A → IO B
fmb =<< ma = ma >>= fmb

_<=<_ : (B -> IO C) -> (A -> IO B) -> A -> IO C
(fmc <=< fmb) a = do
  b ← fmb a
  c ← fmc b
  pure c

void : IO A → IO ⊤
void m = m >> pure tt

sequenceA : List (IO A) → IO (List A)
sequenceA [] = pure []
sequenceA (x ∷ xs) = _∷_ <$> x <*> sequenceA xs
