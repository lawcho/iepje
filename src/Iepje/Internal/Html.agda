
-- User-friendly Html combinators

-- Design priorities:

-- 1. Ergonomic use
-- 2. Easy implementation
-- 3. Enforcement of well-formed HTML

module Iepje.Internal.Html where

open import Iepje.Internal.Utils hiding (_>>_)

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO using (IO; pure)

open import Agda.Builtin.String
open import Agda.Builtin.Sigma
open import Agda.Builtin.List

module _ (event : Set) where
  data Doc : Set where
    tag : String → Doc → Doc
    text : String → Doc
    attr : String → String → Doc -- applies to the *parent* element
    style : String → String → Doc -- applies to the *parent* element
    onIO : (js-event-name : String)
      → (DOM.Event-of js-event-name .fst → IO event)
      → Doc
    append : Doc → Doc → Doc
    empty : Doc

private variable e a b : Set

_>>_ : Doc e → Doc e → Doc e
_>>_ = append
infixl 20 _>>_

on : (js-event-name : String)
    → (DOM.Event-of js-event-name .fst → e)
    → Doc e
on s h = onIO s (pure ∘ h)

on-key-down on-key-up : (String → e) → Doc e
on-key-down decode = onIO "keydown" λ e → decode <$> DOM.key e
on-key-up   decode = onIO "keyup"   λ e → decode <$> DOM.key e

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

-- Change the event type of a Doc
mapDocIO : ∀{a b} → (a → IO b) → Doc a → Doc b
mapDocIO {a} {b} f = go where
  go : Doc a → Doc b
  go (tag t d) = tag t (go d)
  go (text txt) = text txt
  go (attr k v) = attr k v
  go (style k v) = style k v
  go (onIO js-event-name g) = onIO js-event-name (f <=< g)
  go (append d1 d2) = append (go d1) (go d2)
  go empty = empty

forDocIO : ∀{a b} → Doc a → (a → IO b) → Doc b
forDocIO d f = mapDocIO f d

mapDoc : ∀{a b} → (a → b) → Doc a → Doc b
mapDoc f d = mapDocIO (pure ∘ f) d

forDoc : ∀{a b} → Doc a → (a → b) → Doc b
forDoc d f = forDocIO d (pure ∘ f)

-- Map over a list, producing a docs and concatenating them
foldMapDoc : ∀{a b : Set} → (a → Doc b) → List a → Doc b
foldMapDoc f = foldr (λ a d → f a >> d) empty
