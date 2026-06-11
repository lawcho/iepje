
-- Data type of HTML documents
-- Minimal contents, for internal use by renderer

module Iepje.Internal.Doc.Core (event : Set) where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO using (IO)
open import Iepje.Internal.Doc.Has-style

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

data Doc' (ns : String) : Set where
  ns-tag' : (ns' tag-name : String)
    → (DOM.ElementNS-of ns' tag-name → Doc' ns')
    → Doc' ns
  text : String → Doc' ns
  attr : String → String → Doc' ns -- applies to the *parent* element
  style : {{Has-style ns}} → String → String → Doc' ns -- applies to the *parent* element
  onIO : (js-event-name : String) -- applies to the *parent* element
    → (DOM.Event-of js-event-name → IO event)
    → Doc' ns
  doc-onIO : (js-event-name : String) -- applies to the root document
    → (DOM.Event-of js-event-name → IO event)
    → Doc' ns
  append : Doc' ns → Doc' ns → Doc' ns
  empty : Doc' ns

Html = Doc' "http://www.w3.org/1999/xhtml"
Svg = Doc' "http://www.w3.org/2000/svg"
Doc = Html -- For backwards compatiblity
