
-- Metaball graphics

-- Demonstrates svg

module Iepje.Examples.Metaballs where

open import Iepje.Prelude
import Iepje.Internal.JS.WebAPIs.DOM as DOM

record Model : Set where
  field x : Float
  field y : Float
  field r : Float
open Model

data Event : Set where
  move-to : (x y : Float) → Event
  change-r : (Δr : Float) → Event

_+f_ = primFloatPlus
_*f_ = primFloatTimes
infixr 20 _+f_

view : Model → Doc Event
view s = do
  svg do
    style "width" "200px"
    style "height" "200px"
    attr "viewBox" "0 0 200 200"

    -- Define the blobbifying SVG filter,
    -- https://css-tricks.com/gooey-effect/#making-things-stick
    tag "defs" do
      tag "filter" do
        attr "id" "goo"
        tag "feGaussianBlur" do
          attr "in" "SourceGraphic"
          attr "stdDeviation" "10"
          attr "result" "blur"
        tag "feColorMatrix" do
          attr "in" "blur"
          attr "type" "matrix"
          attr "values" "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 18 -7"
          attr "result" "goo"
        tag "feBlend" do
          attr "in" "SourceGraphic"
          attr "in2" "goo"
          attr "mode" "multiply"

    -- Draw some metaballs
    tag "g" do
      style "filter" "url(#goo)"
      tag "circle" do
        attr "cx" "100"
        attr "cy" "100"
        attr "r" "40"
        style "fill" "blue"
      tag "circle" do
        attr "cx" "50"
        attr "cy" "180"
        attr "r" "12"
        style "fill" "blue"
      tag "circle" do
        attr "cx" $ primShowFloat $ s .x
        attr "cy" $ primShowFloat $ s .y
        attr "r" $ primShowFloat $ s .r
        style "fill" "blue"

  -- Handle interaction
  onIO "mousemove" λ e → IO.do
    x ← DOM.get-offsetX (up e)
    y ← DOM.get-offsetY (up e)
    IO.pure (move-to x y)

  onIO "wheel" λ e → IO.do
    Δx ← DOM.get-deltaX (up e)
    Δy ← DOM.get-deltaY (up e)
    Δz ← DOM.get-deltaZ (up e)
    IO.pure $ change-r $ (Δx +f Δy +f Δz) *f 0.02

update : Event → Model → Model
update (move-to x y) m = record m {x = x; y = y}
update (change-r Δr) m = record m {r = m .r +f Δr}

metaballs : IO ⊤
metaballs = interact "#metaball-app"
  (record {x = 150.0; y = 150.0; r = 20.0})
  view
  update