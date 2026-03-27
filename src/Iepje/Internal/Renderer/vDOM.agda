
-- Data structure tracking the state of browser DOM

module Iepje.Internal.Renderer.vDOM where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

data vDOM : Set where
  tag   : (tag-name : String) → (DOM.Element-of tag-name .fst) → vDOM → vDOM
  text  : String → DOM.Text → vDOM
  attr  : String → String → vDOM
  style : String → String → vDOM
  onIO     : (n : String) → DOM.event-listener n → vDOM
  doc-onIO : (n : String) → DOM.event-listener n → vDOM
  append : vDOM → vDOM → vDOM
  empty : vDOM
