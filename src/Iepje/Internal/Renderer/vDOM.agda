
-- Data structure tracking the state of browser DOM

module Iepje.Internal.Renderer.vDOM where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.Doc.Has-style

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

data vDOM (ns : String) : Set where
  tag   : (ns' tag-name : String) → (DOM.ElementNS-of ns' tag-name) → vDOM ns' → vDOM ns
  text  : String → DOM.Text → vDOM ns
  attr  : String → String → vDOM ns
  style : {{Has-style ns}} → String → String → vDOM ns
  with-parent : vDOM ns → vDOM ns
  with-document : vDOM ns → vDOM ns
  onIO     : (target : DOM.EventTarget) (n : String) → DOM.event-listener n → vDOM ns
  append : vDOM ns → vDOM ns → vDOM ns
  empty : vDOM ns
