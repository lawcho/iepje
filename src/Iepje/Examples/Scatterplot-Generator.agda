
-- Performance microbenchmark

-- Also demonstrates serialization to JSON,
-- file download, and https://xkcd.com/688/

module Iepje.Examples.Scatterplot-Generator where

open import Iepje.Prelude as P
  hiding (length; map; for; fst; snd; _,_; _+_; _-_; _*_; _/_; _<_)
  renaming
    ( primFloatPlus to _+_
    ; primFloatMinus to _-_
    ; primFloatTimes to _*_
    ; primFloatDiv to _/_
    ; primFloatLess to _<_
    ; primNatToFloat to nat
    ; primShowFloat to sf
    ; primShowNat to sn
    )
open import Agda.Builtin.Equality
open import Iepje.Internal.Doc.Core using (empty)
open import Iepje.Internal.JS.Language.Union using (_∪_)
open import Iepje.Internal.JS.Language.PrimitiveTypes using (null)
open import Iepje.Internal.JS.WebAPIs.File using (Blob; module Blob-methods)

open import Iepje.Examples.Lib.Array as Array
open import Iepje.Examples.Lib.Float as Float
open import Iepje.Examples.Lib.JSON as JSON using (JSON)

module ToJSON where
  floats-to-json : List Float → JSON
  floats-to-json fs = JSON.array (map JSON.number fs)
open ToJSON

module Download where
  postulate blobbify : String → IO Blob
  {-# COMPILE JS blobbify = s => k => k(new Blob([s])) #-}

  URL = String

  postulate WindowProxy : Set
  postulate open' : URL → IO (WindowProxy ∪ null) -- Unrelaible on some browsers (but degrades to no-op)
  {-# COMPILE JS open' = t => k => k(open(t)) #-}
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
  div do  text $ "Number of points = " ++ sn n
  div do  text $ "Mean of last "++ sn window-width ++ " delays = " ++ toFixed rmd 3 ++ "s"
  tag "button" do
    attr "title" "Click to download data as JSON" 
    onIO "click" λ _ → IO.do
      blob ← blobbify $ JSON.stringify $ floats-to-json ds
      url ← Blob-methods.createObjectURL blob
      open' url
      IO.pure nop
    svg do
      attr "viewBox" $ "0 0 " ++ sf svg-w ++ " " ++ sf svg-h
      attr "width" (sf svg-w); attr "height" (sf svg-h)
      concatDocs $ fori ds λ i delay → do
        if svg-h < to-y delay
          -- Show markers for datapoints that won't fit in the viewport
          then line "pink" (to-x i) (svg-h * 0.95)
                           (to-x i) svg-h
          -- Render datapoints that do fit as circles
          else tag "circle" do
            attr "cx" $ sf $ to-x i
            attr "cy" $ sf $ to-y delay
            attr "r" "1"
            attr "fill" "blue"
            attr "fill-opacity" "20%"
      -- Show recent mean & the window it was calculated from
      line "grey" (to-x 0           ) (to-y rmd)
                  (to-x window-width) (to-y rmd)
  on "click" λ _ → resume
  doc-on "blur" λ _ → pause
  when (not (m .running)) $ div do
    text "Measurement paused, click to resume"
    style "font-style" "italic"
    style "font-size" "50%"
  where
    ds = delays m
    n = length ds
    rmd = mean (take window-width ds)

    to-y : Delay → Y
    to-y d = svg-h * (d / (2.0 * rmd))

    to-x : Index → X
    to-x i = svg-w * (nat i / nat n)

tstep : Float → Model → Model
tstep Δt m@record {running = true} = record m {delays = Δt ∷ m .delays}
tstep Δt m = m

update : Event → Model → Model
update nop m = m
update pause m = record m {running = false}
update resume m = record m {running = true}

fps-cap = 10000 -- large number to not limit performance

scatterplot-generator : IO ⊤
scatterplot-generator = play "#scatterplot-generator-app" fps-cap m0 view update tstep
