-- A module to dump misc. helper functions like `map`

module Iepje.Internal.Utils where

open import Iepje.Internal.JS.Language.IO

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Bool
open import Agda.Builtin.String
open import Agda.Builtin.Sigma
open import Agda.Builtin.Unit
open import Agda.Builtin.Equality

private variable
  ℓa ℓb ℓc : Agda.Primitive.Level
  A : Set ℓa
  B : Set ℓb
  C : Set ℓc
  B' : A → Set ℓb

-- Equality

cong : ∀(f : A → B){a1 a2} → a1 ≡ a2 → f a1 ≡ f a2
cong _ refl = refl

inj-suc : ∀ {m n} → suc m ≡ suc n → m ≡ n
inj-suc refl = refl

==-to-≡ : ∀{m n} → (m == n) ≡ true → m ≡ n
==-to-≡ {zero} {zero} p = refl
==-to-≡ {suc m} {suc n} p = cong suc (==-to-≡ p)

-- Lists

indexed-map : (Nat → A → B) → List A → List B
indexed-map {A = A} {B = B} f = go 0 where
  go : Nat → List A → List B
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

length : List A → Nat
length [] = 0
length (_ ∷ l) = 1 + length l
{-# COMPILE JS length = _ => _ => arr => BigInt (arr.length) #-}

mapi : (Nat → A → B) → List A → List B
mapi = indexed-map

fori : List A → (Nat → A → B) → List B
fori l f = indexed-map f l

foldl : (B → A → B) → B → List A → B
foldl f b [] = b
foldl f b (a ∷ as) = foldl f (f b a) as
{-# COMPILE JS foldl = _ => _ => _ => _ => f => b => arr =>
  arr.reduce
    ( (accumulator,currentValue) => f(accumulator)(currentValue)
    , b
    )
#-}

foldr : (A → B → B) → B → List A → B
foldr f b [] = b
foldr f b (a ∷ as) = f a (foldr f b as)
{-# COMPILE JS foldr = _ => _ => _ => _ => f => b => arr =>
  arr.reduceRight
    ( (accumulator,currentValue) => f(currentValue)(accumulator)
    , b
    )
#-}

map : (A → B) → List A → List B
map f = indexed-map λ _ → f

for : List A → (A → B) → List B
for as f = map f as

-- Legacy aliases
map' = mapi
for' = fori

zipWith : (A → B → C) → (as : List A) (bs : List B) → length as ≡ length bs → List C
zipWith f [] [] _ = []
zipWith f (a ∷ as) (b ∷ bs) p = f a b ∷ zipWith f as bs (inj-suc p)
{-# COMPILE JS zipWith = _ => _ => _ => _ => _ => _ => f => as => bs => _ =>
  as.map((a,i) => f(a)(bs[i]))
#-}

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

-- Control flow

_$_ : ((a : A) → B' a) → (a : A) → B' a
f $ x = f x
infixr 1 _$_

_&_ : (a : A) → ((a : A) → B' a) → B' a
x & f = f x

_∘_ : (B → C) → (A → B) → (A → C)
(g ∘ f) x = g (f x)

infixr 20 _∘_

case_of_ : A → (A → B) → B
case x of f = f x

const : A → B → A
const a _ = a

if_then_else_ : Bool → A → A → A
if true  then t else _ = t
if false then _ else e = e
infixr 20 if_then_else_

-- Nats

enumerate : Nat → List Nat
enumerate zero = []
enumerate (suc n) = n ∷ enumerate n

min : Nat → Nat → Nat
min m n = if m < n then m else n

max : Nat → Nat → Nat
max m n = if m < n then n else m

_/_ : Nat → Nat → Nat
n / m = div-helper 0 (m - 1) n (m - 1)
infixl 22 _/_

_%_ : Nat → Nat → Nat
n % m = mod-helper 0 (m - 1) n (m - 1)
infixl 22 _%_

-- Strings

_++_ = primStringAppend
infixl 20 _++_

-- IO

-- Various generalizations of _$_
-- (Haskell naming convention)

_<$>_ : (A → B) → IO A → IO B
f <$> ma = do a ← ma; pure (f a)
infixr 21 _<$>_

_<$_ : A → IO B → IO A
a <$ fb = const a <$> fb

_<&>_ : IO A → (A → B) → IO B
x <&> f = f <$> x

_>>_ : IO A → IO B → IO B
ma >> mb = do _ ← ma; mb

_<<_ : IO A → IO B → IO A
ma << mb = mb >> ma

_<*>_ : IO (A → B) → IO A → IO B
mf <*> mx = do f ← mf; x ← mx; pure (f x)
infixl 20 _<*>_

_<*_ : IO A → IO B → IO A
fa <* fb = const <$> fa <*> fb

_=<<_ : (A → IO B) → IO A → IO B
fmb =<< ma = ma >>= fmb

_<=<_ : (B -> IO C) -> (A -> IO B) -> A -> IO C
(fmc <=< fmb) a = do
  b ← fmb a
  c ← fmc b
  pure c

-- Helper for type of _$$_
IO? : Bool → Set → Set
IO? false A =    A
IO? true  A = IO A

-- Helper for body of _$$_
from-IO? : ∀ b → IO? b A → IO A
from-IO? false = pure
from-IO? true ma = ma

-- Operator generalizing _$_ _<$>_ _<*>_ _=<<_ flap etc.
_$$_ : ∀{x y z} → IO? x (A → IO? y B) → IO? z A → IO B
_$$_ {x = x}{y = y}{z = z} mfm ma = do
  fm ← from-IO? x mfm
  a ← from-IO? z ma
  from-IO? y (fm a)

infixl 20 _$$_

-- Other IO

void : IO A → IO ⊤
void m = m >> pure tt

sequenceA : List (IO A) → IO (List A)
sequenceA [] = pure []
sequenceA (x ∷ xs) = _∷_ <$> x <*> sequenceA xs
{-# COMPILE JS sequenceA = _ => lkka => kla => kla
  (lkka.map
    ( kka =>
      {
        let r;
        kka(a => r = a);
        return r;
      }
    )
  )
#-}

