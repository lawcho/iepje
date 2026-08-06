
-- Functions for attaching event listeners which return promises

module Iepje.Examples.Lib.Async-Listeners where

open import Iepje.Prelude
open import Iepje.Examples.Lib.Promise as Promise using (Promise)
open import Iepje.Internal.Utils using (_<$>_)

-- Attach asynchronous effectful filtering event listener to arbitrary element
onIO'-Promise-Maybe : ∀{ns e}
  → (target : DOM.EventTarget) (js-event-name : String)
  → (DOM.Event-of js-event-name → IO (Promise (Maybe e)))
  → Doc' e ns
onIO'-Promise-Maybe tgt s pl = with-submit-event λ submit-event → on''' tgt s λ e → IO.do
  pme ← pl e
  Promise.forIO pme λ where
    (just x) → submit-event x
    nothing → IO.pure tt
  IO.pure tt

-- Attach asynchronous effectful event listener to arbitrary element
onIO'-Promise : ∀{ns e}
  → (target : DOM.EventTarget) (js-event-name : String)
  → (DOM.Event-of js-event-name → IO (Promise e))
  → Doc' e ns
onIO'-Promise tgt s pl = with-submit-event λ submit-event → on''' tgt s λ e → IO.do
  pe ← pl e
  Promise.mapIO submit-event pe
  IO.pure tt

-- Attach asynchronous effectful filtering event listener to parent element (or root doc)
onIO-Promise-Maybe doc-onIO-Promise-Maybe : ∀{ns e} (js-event-name : String)
  → (DOM.Event-of js-event-name → IO (Promise (Maybe e)))
  → Doc' e ns
onIO-Promise-Maybe s pml = with-parent λ p → onIO'-Promise-Maybe (up p) s pml
doc-onIO-Promise-Maybe s pml = with-document λ d → onIO'-Promise-Maybe (up d) s pml

-- Attach asynchronous effectful event listener to parent element (or root doc)
onIO-Promise doc-onIO-Promise : ∀{ns e} (js-event-name : String)
  → (DOM.Event-of js-event-name → IO (Promise e))
  → Doc' e ns
onIO-Promise s pl = onIO-Promise-Maybe s λ e → Promise.map just <$> (pl e)
doc-onIO-Promise s pl = doc-onIO-Promise-Maybe s λ e → Promise.map just <$> (pl e)

