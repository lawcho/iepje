
-- Imperative glue code impementing The Elm Architecture

module Iepje.Internal.Runtime where

import      Iepje.Internal.Renderer as Renderer
open import Iepje.Internal.Html as Html using (Doc)

open import Iepje.Internal.Utils

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences
open import Iepje.Internal.JS.Language.SubTyping

open import Agda.Builtin.Unit
open import Agda.Builtin.String
open import Agda.Builtin.List

-- State needed to run a TEA application
record Runtime-Store (model : Set) : Set where
  constructor runtime-store
  -- The current state of the model
  field current-model : Ref model
  -- IO actions to run on each animation tick
  field on-animation-tick-hooks : Ref (List (IO ⊤))
open Runtime-Store

create-Store : ∀{model} → model → IO (Runtime-Store model)
create-Store {model} m0 = runtime-store <$> new m0 <*> new []

-- Run all the hooks in a Runtime-Store, immediately
run-hooks : ∀{model} → Runtime-Store model → IO ⊤
run-hooks rs = do
  get (rs .on-animation-tick-hooks) >>= sequenceA
  pure tt

-- Run all the hooks in a Runtime-Store, on the next browser animation frame
schedule-refresh : ∀{model} → Runtime-Store model → IO ⊤
schedule-refresh rs = do
  -- Currently, this function always schedules a re-render,
  -- even if there is already one pending for the next-frame.
  -- This could be avoided by checking a 'dirty bit' here.
  d ← DOM.document
  w ← DOM.get-defaultView d
  DOM.requestAnimationFrame w λ _ → run-hooks rs
  pure tt

-- Update the model, and schedule a refresh on the next browser animation frame
apply-update-and-schedule-refresh : ∀{model} → Runtime-Store model → (model → IO model) → IO ⊤
apply-update-and-schedule-refresh rs update = do
  m ← get $ rs .current-model
  m' ← update m
  set (rs .current-model) m'
  schedule-refresh rs

-- Add a view to a Runtime-Store, under an existing Element
-- The view is re-rendered from scratch each update
addView
  : ∀{model event}
  → Runtime-Store model
  → (model → IO (Doc event))
  → (event → model → IO model)
  → DOM.HTMLElement
  → IO ⊤
addView rs view update el = do
  -- Add this view's rendering callback to the store
  -- so that updates due to other views will also refresh this view
  modify (rs .on-animation-tick-hooks) (renderThisView ∷_)
  -- Render the initial view (replaces el & sets up callbacks)
  renderThisView
  where
    -- Callback: every animation frame, when the model is dirty...
    renderThisView : IO ⊤
    renderThisView = do
      m ← get $ rs .current-model
      doc ← view m  -- Generate the (declarative) Doc
        -- Modify the Doc's callbacks to re-enter the runtime
        <&> Html.mapDocIO λ e →
          apply-update-and-schedule-refresh rs (update e)
      DOM.replaceChildren (up el) []  -- Clear el
      Renderer.appendInto (up el) doc -- Render into el
