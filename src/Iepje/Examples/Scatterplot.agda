
-- Performance microbenchmark

-- Also demonstrates serialization to JSON,
-- file download, and https://xkcd.com/688/

module Iepje.Examples.Scatterplot where

open import Iepje.Prelude as P
  hiding (max; min; length; map; for; fst; snd; _,_; List; _∷_; [])
open import Agda.Builtin.Equality
open import Iepje.Internal.Doc.Core using (empty)
open import Iepje.Internal.JS.Language.Union using (_∪_)
open import Iepje.Internal.JS.Language.PrimitiveTypes using (null)

module ScalarOps where
  postulate fmin fmax : Float → Float → Float
  {-# COMPILE JS fmax = f1 => f2 => Math.max(f1, f2) #-}
  {-# COMPILE JS fmin = f1 => f2 => Math.min(f1, f2) #-}

  _/f_ = primFloatDiv
  _*f_ = primFloatTimes
  _+f_ = primFloatPlus
  _<f_ = primFloatLess
  nat = primNatToFloat
  sf = primShowFloat
  sn = primShowNat

  postulate toFixed : Float → Nat → String
  {-# COMPILE JS toFixed = f => n => f.toFixed(Number(n)) #-}
open ScalarOps

-- Fast & safe list library
module ListOps where
  -- Custom type to work around agda#8639
  data List (A : Set) : Set where
    [] : List A
    _∷_ : A → List A → List A
  infixr 5 _∷_

  {-# COMPILE JS List = ((x,v) => (x.length < 1) ? v["[]"]() : v["_∷_"](x[0], x.slice(1))) #-}
  {-# COMPILE JS [] = Array() #-}
  {-# COMPILE JS _∷_ = x => y => [x].concat(y) #-}

  toBuiltinList : ∀{A} → List A → P.List A
  toBuiltinList [] = P.[]
  toBuiltinList (x ∷ l) = x P.∷ toBuiltinList l
  {-# COMPILE JS toBuiltinList = _ => arr => arr #-}

  length : ∀{A} → List A → Nat
  length [] = 0
  length (_ ∷ l) = 1 + length l
  {-# COMPILE JS length = _ => arr => BigInt (arr.length) #-}

  take : ∀{A} → Nat → List A → List A
  take _ [] = []
  take zero _ = []
  take (suc n) (x ∷ l) = x ∷ take n l
  {-# COMPILE JS take = _ => n => arr => arr.slice(0,Number(n)) #-}

  asum : List Float → Float
  asum [] = 0.0
  asum (x ∷ l) = x +f asum l
  {-# COMPILE JS asum = arr => Math.sumPrecise(arr) #-}

  mean : List Float → Float
  mean l = asum l /f nat (length l)

  map : ∀{A B} → (A → B) → List A → List B
  map f [] = []
  map f (a ∷ as) = f a ∷ (map f as)
  {-# COMPILE JS map = _ => _ => f => arr => arr.map(f) #-}

  mapi : ∀{A B} → (Nat → A → B) → List A → List B
  mapi {A}{B} f = go 0 where
    go : Nat → List A → List B
    go _ [] = []
    go n (x ∷ l) = f n x ∷ go (1 + n) l
  {-# COMPILE JS mapi = _ => _=> f => arr => arr.map((x,i) => f(i)(x)) #-}

  fori : ∀{A B} → List A → (Nat → A → B) → List B
  fori l f = mapi f l
open ListOps

module ToJson where
  JSON = String
  postulate floats-to-json : List Float → JSON
  {-# COMPILE JS floats-to-json = fs => JSON.stringify(fs) #-}
open ToJson

module Download where
  postulate Blob : Set
  postulate blobbify : String → IO Blob
  {-# COMPILE JS blobbify = s => k => k(new Blob([s])) #-}

  URL = String

  postulate createObjectURL : Blob → IO URL
  {-# COMPILE JS createObjectURL = b => k => k (URL.createObjectURL(b)) #-}

  postulate WindowProxy : Set
  postulate open-window : URL → IO (WindowProxy ∪ null) -- Unrelaible on some browsers (but degrades to no-op)
  {-# COMPILE JS open-window = t => k => k(open(t)) #-}
open Download

-- MVU, plotting, & misc.

Delay = Float -- in seconds
Index = Nat -- position in a list
X = Float -- in SVG units
Y = Float -- in SVG units
Color = String

record Model : Set where
  field running : Bool
  field delays : List Delay
open Model

m0 : Model
m0 .running = false
m0 .delays = []

data Event : Set where
  nop : Event
  pause : Event
  resume : Event

window-width = 80 -- for calculating rolling mean

svg-w : X; svg-w = 250.0
svg-h : Y; svg-h = 100.0

line : Color → X → Y → X → Y → Svg Event
line c x1 y1 x2 y2 = tag "line" do
  attr "x1" $ sf $ x1
  attr "y1" $ sf $ y1
  attr "x2" $ sf $ x2
  attr "y2" $ sf $ y2
  style "stroke" c

view : Model → Html Event
view m = do
  div do  text $ "Number of points = " ++ primShowNat n
  div do  text $ "Mean of last "++ sn window-width ++ " delays = " ++ toFixed rmd 3 ++ "s"
  tag "button" do
    attr "title" "Click to download data as JSON" 
    onIO "click" λ _ → IO.do
      blob ← blobbify $ floats-to-json ds
      url ← createObjectURL blob
      open-window url
      IO.pure nop
    svg do
      style "grid-area" "1 / 1"
      attr "viewBox" $ "0 0 " ++ sf svg-w ++ " " ++ sf svg-h
      attr "width" (sf svg-w); attr "height" (sf svg-h)
      concatDocs $ toBuiltinList $ fori ds λ i delay → do
        let x = to-x i
        let y = to-y delay
        if svg-h <f y
          -- Show markers for datapoints that won't fit in the viewport
          then line "pink" x (svg-h *f 0.95)
                           x svg-h
          -- Render datapoints that do fit as circles
          else tag "circle" do
            attr "cx" $ sf $ to-x i
            attr "cy" $ sf $ to-y delay
            attr "r" "1"
            attr "fill" "blue"
            attr "fill-opacity" "20%"
      line "grey" (to-x 0           ) (to-y rmd)
                  (to-x window-width) (to-y rmd)
  on "click" λ _ → resume
  doc-on "blur" λ _ → pause
  when (not (m .running)) $ div do
    text "Measurement paused, click to resume"
    style "font-style" "italic"
    style "font-size" "50%"
    style "grid-area" "1 / 1"
  where
    ds = delays m
    n = length ds
    rmd = mean (take window-width ds)

    to-y : Delay → X
    to-y d =  svg-h *f (d /f (2.0 *f rmd))

    to-x : Index → Y
    to-x i = svg-w *f (nat i /f nat n)

tstep : Float → Model → Model
tstep Δt m@record {running = true} = record m {delays = Δt ∷ m .delays}
tstep Δt m = m

update : Event → Model → Model
update nop m = m
update pause m = record m {running = false}
update resume m = record m {running = true}

fps-cap = 10000 -- large number to not limit performance

scatterplot : IO ⊤
scatterplot = play "#scatterplot-app" fps-cap m0 view update tstep
