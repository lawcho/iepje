
-- User-friendly combinators for writing Doc s

module Iepje.Internal.Doc.Combinators where

open import Iepje.Internal.Utils hiding (_>>_)

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO using (IO; pure)
open import Iepje.Internal.JS.Language.SubTyping using (up)

open import Iepje.Internal.Doc.Core
open import Iepje.Internal.Doc.Has-style
open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.Sigma
open import Agda.Builtin.Bool

private variable ns : String
private variable e a b : Set

_>>_ : ∀{ns} → Doc' e ns → Doc' e ns → Doc' e ns
_>>_ = append
infixl 20 _>>_

on doc-on : ∀{ns} → (js-event-name : String)
    → (DOM.Event-of js-event-name → e)
    → Doc' e ns
on s h = onIO s (pure ∘ h)
doc-on s h = doc-onIO s (pure ∘ h)

on-key-down on-key-up : ∀{ns} → (String → e) → Doc' e ns
on-key-down decode = onIO "keydown" λ e → decode <$> DOM.key (up e)
on-key-up   decode = onIO "keyup"   λ e → decode <$> DOM.key (up e)

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
mapDocIO {ns} {a} {b} f = go where
  go : ∀{ns} → Doc' a ns → Doc' b ns
  go (ns-tag' ns t f) = ns-tag' ns t λ e → (go (f e))
  go (text txt) = text txt
  go (attr k v) = attr k v
  go (style k v) = style k v
  go (onIO js-event-name g) = onIO js-event-name (f <=< g)
  go (doc-onIO js-event-name g) = doc-onIO js-event-name (f <=< g)
  go (append d1 d2) = append (go d1) (go d2)
  go empty = empty

forDocIO : ∀{a b} → Doc a → (a → IO b) → Doc b
forDocIO d f = mapDocIO f d

mapDoc : ∀{a b} → (a → b) → Doc a → Doc b
mapDoc f d = mapDocIO (pure ∘ f) d

forDoc : ∀{a b} → Doc a → (a → b) → Doc b
forDoc d f = forDocIO d (pure ∘ f)

when : Bool → Doc a → Doc a
when true a = a
when false _ = empty 

concatDocs : List (Doc' a ns) → Doc' a ns
concatDocs = foldr _>>_ empty
