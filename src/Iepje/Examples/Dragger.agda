
-- Widget with drag-and-drop divider

-- Demonstrates beacons, focus retention despite varying size

-- https://blog.noredink.com/post/186724971283/drag-drop-without-draggables-dropzones

module Iepje.Examples.Dragger where

open import Iepje.Prelude

grid-box : ∀{e} → Nat → Nat → Nat → Nat → Doc e
grid-box x₀ x₁ y₀ y₁ = do
  style "grid-column-start" $ primShowNat x₀
  style "grid-column-end"   $ primShowNat x₁
  style "grid-row-start"    $ primShowNat y₀
  style "grid-row-end"      $ primShowNat y₁

insertion-list : ∀ {e} → Bool → Bool → (Maybe Nat → e) → List (Doc e) → (Nat → Doc e) → Doc e
insertion-list enabled debug hover docs divider = do
    style "display" "grid"
    -- List items
    concatDocs $ for' docs λ i d → do
      div do d; grid-box 0 1 (3 * i + 2) (3 * i + 4)  -- 2 cells
    -- Dividers & hitboxes
    let l = length docs
    concatDocs $ for (enumerate (1 + l)) λ i → do
      div do  -- Divider, covers 1 cell between items
        grid-box 0 1 (3 * i + 1) (3 * i + 2)
        divider i
      -- Hitbox, covering divider & half of each neighbouring item
      when enabled $ tag' "div" λ el → do
        grid-box 0 1 (max 1 (3 * i + 0)) (min (3 * l + 2) (3 * i + 3))
        when debug do
          style "opacity" "20%"
          style "background" "green"
          style "box-shadow" "inset 0 0 0 1px red"
        doc-onIO "mousemove" λ me → IO.do
            my ← DOM.get-clientY (up me)
            er ← DOM.getBoundingClientRect (up el)
            ey-min ← DOM.DOMRect-methods.get-top er
            ey-max ← DOM.DOMRect-methods.get-bottom er
            let in-range = primFloatLess ey-min my && primFloatLess my ey-max
            IO.pure (hover (if in-range then just i else nothing))
            -- This fires l events every mouse movement. The filtering happens later, in update.

data Event : Set where
  drag-over : Maybe Nat → Event
  cancel-drag : Event
  lift-marker : Event
  drop-marker : Event
  toggle-debug : Event

record Model : Set where
  field items : List String
  field marker-pos : Nat
  field caret-pos : Maybe Nat
  field debug : Bool
open Model

dragging : Model → Bool
dragging m = case m .caret-pos of λ where
  (just _) → true
  nothing → false

view : Model → Doc Event
view m = col do
  doc-on "mouseup" λ _ → drop-marker
  doc-on "blur" λ _ → cancel-drag

  let mi = m .marker-pos
  let ci = case m .caret-pos of λ where (just ci) → ci; nothing → mi
  insertion-list (dragging m) (debug m) drag-over
    (for' (m .items) λ i n → do -- Cells
      style "padding" "0.1cm"
      style "border-radius" "0.5cm"
      style "border" "0.05cm solid black"
      style "display" "inline-grid"
      style "grid-template-columns" "1ch auto 1ch"
      when (dragging m) do style "user-select" "none"
      when (i < mi) do style "opacity" "50%"
      when (ci < 1 + i && i < mi || mi < 1 + i && i < ci) do span do
        style "grid-column" "1"
        style "justify-self" "right"
        text "⋆"
      span do
        style "grid-column" "2"
        text n
      do -- Hidden toggle for debug mode
        attr "tabindex" "-1"  -- listen for keypresses
        on "keydown" λ _ → toggle-debug
    )
    (λ i → do  -- Markers
      when (i == mi) do  -- Main marker
        style "height" "0.4cm"
        style "background" (if dragging m then "lightgrey" else "darkgrey")
        on "mousedown" λ _ → lift-marker
      when (i == ci) do -- Insertion caret
        style "box-shadow" "inset 0 0 0 0.1cm darkgrey"
      style "min-height" "0.1cm"
      style "margin" "0.05cm"
      style "user-select" "none"
    )
  when (debug m) do
    style "box-shadow" "inset 0 0 0 1px red"
    span do
      text "Debug mode (click to exit)"
      on "click" λ _ → toggle-debug
      style "font-size" "80%"; style "font-style" "italic"
      style "width" "0"; style "min-width" "100%" -- https://stackoverflow.com/a/56722703


update : Event → Model → Model
update (drag-over (just x))
  r@(record  {caret-pos = just _})
  = record r {caret-pos = just x}
update drop-marker
  r@(record  {caret-pos = just x})
  = record r {marker-pos = x; caret-pos = nothing}
update cancel-drag   r = record r {caret-pos = nothing} --cancel drag
update lift-marker r = record r {caret-pos = just (r .marker-pos)}
update toggle-debug r = record r {debug = not (r .debug)}
update _ r = r

m0 : Model
m0 .items = "apricot" ∷ "bannana" ∷ "cherry" ∷ "grapefruit" ∷ "pear" ∷ "quince" ∷ []
m0 .marker-pos = 2
m0 .caret-pos = nothing
m0 .debug = false

dragger : IO ⊤
dragger = interact "#dragger-app" m0 view update

