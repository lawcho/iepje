
-- A performance stress-test
-- Also demonstrates custom JS bindings

module Iepje.Examples.Conway where

open import Iepje.Prelude hiding (_&_; _%_)
open import Agda.Builtin.Int
open import Agda.Builtin.Bool

module BitVectorLib where
  postulate BitVector : Set -- JS BigInt
  -- Agda's Nat also compiles to a BigInt,
  -- but a BitVector may store bit patterns
  -- not corresponding to any Agda Nat

  postulate fromNat : Nat → BitVector
  {-# COMPILE JS fromNat = n => n #-}

  postulate 0< : BitVector → Bool
  {-# COMPILE JS 0< = bv => (0 < bv) #-}

  postulate _&_ : BitVector → BitVector → BitVector
  {-# COMPILE JS _&_ = bv1 => bv2 => (bv1 & bv2) #-}

  postulate _∣_ : BitVector → BitVector → BitVector
  {-# COMPILE JS _∣_ = bv1 => bv2 => (bv1 | bv2) #-}

  postulate _^_ : BitVector → BitVector → BitVector
  {-# COMPILE JS _^_ = bv1 => bv2 => (bv1 ^ bv2) #-}

  postulate _<<_ : BitVector → Nat → BitVector
  {-# COMPILE JS _<<_ = bv => n => (bv << n) #-}

  1n = fromNat 1

  get-bit : Nat → BitVector → Bool
  get-bit i bv = 0< ((1n << i) & bv)

  set-bit flip-bit clear-bit : Nat → BitVector → BitVector
  set-bit i   = _∣ (1n << i)
  flip-bit i  = _^ (1n << i)
  clear-bit i = flip-bit i ∘ set-bit i

module IntLib where
  -- "True modulo", which always returns a Nat smaller than the 2nd arg.
  postulate _%_ : Int → Nat → Nat
  {-# COMPILE JS _%_ = a => b => (((a % b) + b) % b) #-}

  -- Integer increment/decrement (non destructive)
  postulate inc dec : Int → Int
  {-# COMPILE JS inc = i => (i + 1n) #-}
  {-# COMPILE JS dec = i => (i - 1n) #-}

open BitVectorLib
open IntLib

-- width = 60; height = 60 -- "Too much recursion"
-- width = 50; height = 50 -- Severe lag
-- width = 40; height = 40
-- width = 30; height = 30
width = 20; height = 20
-- width = 10; height = 10

1DIndex = Nat

-- Map unbounded 2D coordinates onto positions in a finite bit-vector
flatten : Int → Int → 1DIndex
flatten x y = xn + yn * width where  -- diagonalize torus
  xn = x % width  -- x coordinate on torus
  yn = y % height -- y coordinate on torus

-- Gridifed BitVector manipulators, for GoL calcualtions
get : Int → Int → BitVector → Bool
get x y = get-bit (flatten x y)

set : Int → Int → BitVector → BitVector
set x y = set-bit (flatten x y)

clear : Int → Int →  BitVector → BitVector
clear x y = clear-bit (flatten x y)

count : List Bool → Nat
count [] = 0
count (true ∷ bs) = 1 + count bs
count (false ∷ bs) = count bs

neighbours : Int → Int → BitVector → List Bool
neighbours x y bv =
  get (dec x) (dec y) bv ∷ get x (dec y) bv ∷ get (inc x) (dec y) bv ∷
  get (dec x)      y  bv ∷                    get (inc x)      y  bv ∷
  get (dec x) (inc y) bv ∷ get x (inc y) bv ∷ get (inc x) (inc y) bv ∷
  []

id : ∀{A : Set} → A → A
id a = a

compose : ∀{A : Set} → List (A → A) → A → A
compose [] = id
compose (f ∷ fs) = f ∘ compose fs

countdown : Nat → List Nat
countdown zero = []
countdown (suc n) = n ∷ countdown n

step : BitVector → BitVector
step bv =
  (compose ∘ for (countdown width) $ λ x →
    compose ∘ for (countdown height) $ λ y →
      case count (neighbours (pos x) (pos y) bv) of λ where
       0 → clear (pos x) (pos y)
       1 → clear (pos x) (pos y)
       2 → id
       3 → set   (pos x) (pos y)
       _ → clear (pos x) (pos y)
  ) bv

view-bit : BitVector → 1DIndex → Doc 1DIndex
view-bit bv i = button i do
  style "background" $
    if BitVectorLib.get-bit i bv
    then "green" else "black"
  -- To debug neighbour calculation:
  -- text $ primShowNat $ count $ neighbours (pos (pos i % width)) (pos (i / width)) bv
  -- style "color" "red"

view-bits : 1DIndex → BitVector → Doc 1DIndex
view-bits zero bv = empty
view-bits (suc n) bv = view-bits n bv >> view-bit bv n

view : BitVector → Doc 1DIndex
view bv = do
  style "display" "grid"
  style "grid-template-rows" ("repeat(" ++ (primShowNat width) ++ ", 1cm")
  style "grid-template-columns" ("repeat(" ++ (primShowNat height) ++ ", 1cm")
  view-bits (width * height) bv

conway : IO ⊤
conway = play "#conway-game"
  10  -- frames per second
  (fromNat 251449965816602612089493083786575604950588208744380557571566445054919399678109225198068535917041267078376366364862136085867197840355123458367268291673924054912727772024587137380606296333842307273582689768241136643120171583984907629635593068860204167434618217140185659852634066267830039257813973293764288328214181458709072990322774443985232952186534351314012000289569634696261766243151210502868197032639089981636484223517247944692769468363619499099330753327468724274914880245105565460681270699673700889726364603343318675226584677260301382994295492895527175159510593406071965281861225685067775103222916780192779898908882905239516075708524684591073276065283015279043566290299618873010602532985655684885915482443476914914833608303672828277453031855366463801311890303771211414673498179800043106907689829953665640449020561187872496219522460945032088627027940842647419276702252022372433629520596625712497901994148036438231028858127073550625380555620199258439624261242712387622)
  view
  flip-bit
  (λ _ → step)
