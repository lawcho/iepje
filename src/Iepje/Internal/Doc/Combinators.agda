
-- User-friendly combinators for writing Doc s

module Iepje.Internal.Doc.Combinators where

open import Iepje.Internal.Utils hiding (_>>_)

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO as IO using (IO; pure)
open import Iepje.Internal.JS.Language.SubTyping using (up)

open import Iepje.Internal.Doc.Core
open import Iepje.Internal.Doc.Has-style
open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.Sigma
open import Agda.Builtin.Bool
open import Agda.Builtin.Maybe
open import Agda.Builtin.Unit

private variable ns : String
private variable e a b : Set

_>>_ : ∀{ns} → Doc' e ns → Doc' e ns → Doc' e ns
_>>_ = append
infixl 20 _>>_

-- Attach synchronous effectful filtering event listener to arbitrary element
onIO'-Maybe : ∀{ns e}
  → (target : DOM.EventTarget) (js-event-name : String)
  → (DOM.Event-of js-event-name → IO (Maybe e))
  → Doc' e ns
onIO'-Maybe tgt s l = with-submit-event λ submit-event → on''' tgt s λ e →
  l e IO.>>= λ where
    (just v) → submit-event v
    nothing → pure tt

-- Attach synchronous effectful event listener to arbitrary element
onIO' : ∀{ns e}
  → (target : DOM.EventTarget) (js-event-name : String)
  → (DOM.Event-of js-event-name → IO e)
  → Doc' e ns
onIO' tgt s l = onIO'-Maybe tgt s λ e → just <$> l e

-- Attach synchronous pure listener to arbitrary element
on' : ∀{ns} → (target : DOM.EventTarget) (js-event-name : String)
    → (DOM.Event-of js-event-name → e)
    → Doc' e ns
on' tgt s l = onIO' tgt s (pure ∘ l)

-- Attach synchronous effectful listener to parent element (or root doc.)
onIO doc-onIO : ∀{ns} → (js-event-name : String)
    → (DOM.Event-of js-event-name → IO e)
    → Doc' e ns
onIO s l = with-parent λ p → onIO' (up p) s l
doc-onIO s l = with-document λ d → onIO' (up d) s l


-- Attach synchronous effectful filtering listener to parent element (or root doc.)
onIO-Maybe doc-onIO-Maybe : ∀{ns} → (js-event-name : String)
    → (DOM.Event-of js-event-name → IO (Maybe e))
    → Doc' e ns
onIO-Maybe s l = with-parent λ p → onIO'-Maybe (up p) s l
doc-onIO-Maybe s l = with-document λ d → onIO'-Maybe (up d) s l

-- Attach synchronous pure listener to parent element (or root doc.)
on doc-on : ∀{ns} → (js-event-name : String)
    → (DOM.Event-of js-event-name → e)
    → Doc' e ns
on s h = onIO s (pure ∘ h)
doc-on s h = doc-onIO s (pure ∘ h)

on-key-down on-key-up : ∀{ns} → (String → Maybe e) → Doc' e ns
on-key-down decode = onIO-Maybe "keydown" λ e → decode <$> DOM.key (up e)
on-key-up   decode = onIO-Maybe "keyup"   λ e → decode <$> DOM.key (up e)

-- Same namespace, passes element
tag' : ∀{ns} t → (DOM.ElementNS-of ns t → Doc' e ns) → Doc' e ns
tag' t d = ns-tag' _ t d

-- Changes namespace, no access to element
ns-tag : ∀{ns} ns' → String → Doc'  e ns' → Doc' e ns
ns-tag ns t d = ns-tag' ns t λ _ → d

-- Same namespace, no access to element
tag : ∀{ns} → String → Doc' e ns → Doc' e ns
tag t d = ns-tag _ t d

div : Doc e → Doc e
div = tag "div"

span : Doc e → Doc e
span = tag "span"

button : e → Doc e → Doc e
button e inner = tag "button" do
  on "click" λ _ → e
  inner

table : Doc e → Doc e
table = tag "table"

tr : Doc e → Doc e
tr = tag "tr"

td : Doc e → Doc e
td = tag "td"

row : Doc e → Doc e
row inner = div do
  style "display" "flex"
  style "flex-direction" "row"
  inner

col : Doc e → Doc e
col inner = div do
  style "display" "flex"
  style "flex-direction" "column"
  inner

br : Doc e
br = tag "br" empty

svg : Svg e → Doc e
svg s = ns-tag _ "svg" s

-- Change the event type of a Doc
mapDocIO : ∀{ns a b} → (a → IO b) → Doc' a ns → Doc' b ns
{-# TERMINATING #-} -- Agda bug: does not work if placed directly on go
mapDocIO {ns} {a} {b} f = go where
  go : ∀{ns} → Doc' a ns → Doc' b ns
  go (with-submit-event g) = with-submit-event λ submit-event →
    go (g (submit-event <=< f))
  go (ns-tag' ns t f) = ns-tag' ns t λ e → (go (f e))
  go (text txt) = text txt
  go (attr k v) = attr k v
  go (style k v) = style k v
  go (with-parent g) = with-parent λ p → go (g p)
  go (with-document g) = with-document λ d → go (g d)
  go (on''' tgt js-event-name g) = on''' tgt js-event-name g
  go (append d1 d2) = append (go d1) (go d2)
  go empty = empty
  go (array ds) = array (map go ds)

forDocIO : ∀{a b} → Doc a → (a → IO b) → Doc b
forDocIO d f = mapDocIO f d

mapDoc : ∀{a b} → (a → b) → Doc a → Doc b
mapDoc f d = mapDocIO (pure ∘ f) d

forDoc : ∀{a b} → Doc a → (a → b) → Doc b
forDoc d f = forDocIO d (pure ∘ f)

when : Bool → Doc' a ns → Doc' a ns
when true a = a
when false _ = empty 

concatDocs : List (Doc' a ns) → Doc' a ns -- Legacy
concatDocs = array
-- concatDocs = foldr _>>_ empty
