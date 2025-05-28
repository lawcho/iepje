
-- Data structure tracking the state of browser DOM

module Iepje.Internal.Renderer.vDOM (event : Set) where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM

open import Agda.Builtin.String

-- This data type follows the structure of
--    Iepje.Internal.Doc.Core.Doc
-- for easy generation & merging,
-- but additionally stores extra pointers into the browser DOM,
-- for easy imperative update of the browser DOM during merging

data vDOM : Set where
  tag   : DOM.HTMLElement → String → vDOM → vDOM
  text  : DOM.Text → String → vDOM
  attr  : String → String → vDOM
  style : String → String → vDOM
  onIO  : (n : String) → DOM.event-listener n → vDOM
  append : vDOM → vDOM → vDOM
  empty : vDOM
