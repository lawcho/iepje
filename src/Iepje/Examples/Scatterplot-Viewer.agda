
-- Viewer for scatterplots porduced by the other example

-- Demonstrates multiply-firing event listener

module Iepje.Examples.Scatterplot-Viewer where

open import Iepje.Prelude
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
open import Iepje.Internal.JS.Language.FromUnion

open import Iepje.Examples.Lib.Async-Listeners as Listen using ()
open import Iepje.Examples.Lib.Promise as Promise using (Promise)
open import Iepje.Examples.Lib.From-Blob as Blob using ()
open import Iepje.Examples.Lib.JSON as JSON using (JSON)
open import Iepje.Examples.Lib.Array as Array
open import Iepje.Examples.Lib.Float as Float

open import Iepje.Internal.JS.WebAPIs.DOM as DOM using (module HTMLElement-methods)
open import Iepje.Internal.JS.WebAPIs.File as File using (module FileList-methods; module File-methods)

open import Iepje.Examples.Lib.Id as Id using ()
open import Iepje.Examples.Lib.Maybe as Maybe using ()

open import Iepje.Internal.Utils using (_<$>_; sequenceA)
open IO using (_>>=_)

module JsonValidation where
  all : ∀{A B : Set} → (A → Maybe B) → List A → Maybe (List B)
  all f = Array.foldr (λ a → Maybe.liftA2 _∷_ (f a)) (just [])

  validate-Float : JSON → Maybe Float
  validate-Float (JSON.number f) = just f
  validate-Float _ = nothing

  validate-Floats : JSON → Maybe (List Float)
  validate-Floats (JSON.array l) = all validate-Float l
  validate-Floats _ = nothing
open JsonValidation

-- MVU, plotting & misc.

FileName = String

record Sequence : Set where
  constructor mk-Sequence
  field name : String
  field points : List Float
  field visible : Bool
open Sequence

data Event : Set where
  malformed-json : FileName → Event
  json-but-not-series : FileName → Event
  loaded-sequence : Sequence → Event
  toggle-visible : Nat → Event

record Model : Set where
  field sequences : List Sequence
  field last-event : Maybe Event

m0 : Model
m0 .Model.sequences = []
m0 .Model.last-event = nothing

update : Event → Model → Model
update e@(loaded-sequence s) m@record {sequences = ss} =
  record {sequences = snoc ss s; last-event = just e}
update e@(toggle-visible i) m@record {sequences = ss} =
  record
    { sequences = update-at i (λ r → record r {visible = not (visible r)}) ss
    ; last-event = just e
    }
update e m = record m {last-event = just e}

DX = Float -- in Data-units
DY = Float -- in Data-units
SX = Float -- in SVG units
SY = Float -- in SVG units
svg-w : SX; svg-w = 250.0
svg-h : SY; svg-h = 100.0

Color = String

line : Color → SX → SY → SX → SY → Svg Event
line c x1 y1 x2 y2 = tag "line" do
  attr "x1" $ sf $ x1
  attr "y1" $ sf $ y1
  attr "x2" $ sf $ x2
  attr "y2" $ sf $ y2
  style "stroke" c

text' : String → SX → SY → Svg Event
text' msg x y = tag "text" do
  attr "x" (sf x)
  attr "y" (sf y)
  text msg

-- Lines across the whole SVG viewport
hline : Color → SX → Svg Event
vline : Color → SY → Svg Event
hline c y = line c 0.0 y svg-w y
vline c x = line c x 0.0 x svg-h

-- Round a float to a nat, returning 0 for NaNs & negatives
to-Nat : Float → Nat
to-Nat f with primFloatRound f
... | just (pos n) = n
... | just (negsuc _) = 0
... | nothing = 0

-- Select axis-tick points for a 1D data-set with maximum value x
-- The ticks are placed at intervals of 1/5 of some power of 10,
-- such that x is included, and mimimal space is wasted above x
ticks-for : Float → Array Float
ticks-for x = let
    d = primFloatPow 10.0 (Float.floor (Float.log10 x))
    n = Float.ceil (x / d)
  in Array.tabulate (suc (to-Nat n)) λ i → nat i * d

-- Some color palettes designed for displaying categorical data.

-- https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=8
palette-cb-dark2 =
  "#1b9e77" ∷
  "#d95f02" ∷
  "#7570b3" ∷
  "#e7298a" ∷
  "#66a61e" ∷
  "#e6ab02" ∷
  "#a6761d" ∷
  "#666666" ∷
  []

-- https://colorbrewer2.org/#type=qualitative&scheme=Pastel2&n=8
palette-cb-pastel2 =
  "#b3e2cd" ∷
  "#fdcdac" ∷
  "#cbd5e8" ∷
  "#f4cae4" ∷
  "#e6f5c9" ∷
  "#fff2ae" ∷
  "#f1e2cc" ∷
  "#cccccc" ∷
  []

color : Nat → Color
color i = get-with-default "black" i palette-cb-pastel2

view-seq : Sequence → Html Event
view-seq s = span do
  text $ name s
  style "font-family" "monospace"

view : Model → Html Event
view m = col do
  row do
    -- Data view
    svg do
      attr "viewBox" $ "0 0 " ++ sf svg-w ++ " " ++ sf svg-h
      attr "width" (sf svg-w); attr "height" (sf svg-h)
      -- Draw the points
      concatDocs $ fori sequences λ i s → when (s .visible) do
        concatDocs $ fori (points s) λ x y →
          tag "circle" do
              attr "cx" $ sf $ to-sx (nat x)
              attr "cy" $ sf $ to-sy y
              attr "r" "1"
              attr "fill" (color i)
              -- Ensure overlapping points mix color, rather than some covering others
              style "mix-blend-mode" "darken"
      hline "black" (to-sy 0.0) -- X axis
      vline "black" (to-sx 0.0) -- Y axis
      concatDocs $ for (ticks-for max-dy) λ tick-dy → tag "g" do  -- ticks
        hline "rgba(0, 0, 0, 0.2)" (to-sy tick-dy)
        text' (toExponential tick-dy 0) 0.0 (to-sy tick-dy)
    -- Legend
    div do
      style "display" "grid"
      style "align-items" "center"
      style "grid-template-columns" "0.5cm auto"
      style "grid-auto-rows" "0.5cm"
      style "grid-row-gap" "0.1cm"
      style "grid-column-gap" "0.1cm"
      style "margin" "0.1cm"
      concatDocs $ fori sequences λ i s → do
        div do
          style "background" (color i)
          style "border-radius" "30%"
          style "align-self" "stretch"
          when (not (s .visible)) do
            style "opacity" "50%"
            style "text-align" "center"
            text "h"
          on "click" λ _ → toggle-visible i
        view-seq s
      -- Upload form
      tag' "input" λ el → do
        style "grid-column" "1 / 3"
        attr "type" "file"
        attr "multiple" "true"
        with-submit-event λ submit-event → on''' (up el) "change" λ _ → IO.do
          fs ← FileList-methods.to-List <$> HTMLElement-methods.get-files (up el)
          sequenceA $ Array.for fs λ f → IO.do
            -- Add all the series, as soon as they are ready
            fn ← File-methods.get-name f
            p-contents ← Blob.text (up f)
            Promise.forIO p-contents λ contents →
              submit-event $ Id.do
                (just j) ← JSON.parse contents where nothing → malformed-json fn
                (just ps) ← validate-Floats j where nothing → json-but-not-series fn
                loaded-sequence $ mk-Sequence fn ps true
          IO.pure tt
  -- Info message
  case last-event of λ where
    nothing → text "Please select JSON files to upload."
    (just (malformed-json fn))     → text $ "Malformed JSON in " ++ fn
    (just (json-but-not-series fn)) → text $ "Unexpected JSON structure in " ++ fn
                                          ++ " (expected a list of floats)"
    (just (loaded-sequence s)) → text $ "Succesfully loaded sequence from " ++ (s . name)
    (just (toggle-visible i)) → span do
      text "Toggled visiblity of series "
      lookup (span do text do "(error in lookup)") view-seq i sequences
  where
    open Model m

    visible-yss : List (List DY)
    visible-yss = map points $ filter visible $ sequences

    avoid-NaN = if length visible-yss == 0 then (λ x → 0.0) else (λ x → x)

    min-dx = 0.0
    max-dx = Array.max $ map (nat ∘ length) visible-yss
    min-dy = Array.min $ map Array.min visible-yss
    max-dy = Array.max $ map Array.max visible-yss

    to-sy : DY → SY
    to-sy dy = avoid-NaN $ svg-h * (1.0 - ((dy - min-dy) / (max-dy - min-dy)))

    to-sx : DX → SX
    to-sx dx = avoid-NaN $ svg-w * ((dx - min-dx) / (max-dx - min-dx))

scatterplot-viewer : IO ⊤
scatterplot-viewer = interact "#scatterplot-viewer-app" m0 view update
