
-- Data type of HTML documents
-- Minimal contents, for internal use by renderer

module Iepje.Internal.Doc.Core (event : Set) where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO using (IO)
open import Iepje.Internal.Doc.Has-style

open import Agda.Builtin.Unit 
open import Agda.Builtin.String
open import Agda.Builtin.List

data Doc' (ns : String) : Set where
  ns-tag' : (ns' tag-name : String)
    → (DOM.ElementNS-of ns' tag-name → Doc' ns')
    → Doc' ns
  text : String → Doc' ns
  attr : String → String → Doc' ns -- applies to the *parent* element
  style : {{Has-style ns}} → String → String → Doc' ns -- applies to the *parent* element
  with-parent : (DOM.Element → Doc' ns) → Doc' ns
  with-document : (DOM.Document → Doc' ns) → Doc' ns
  with-submit-event : ((event → IO ⊤) → Doc' ns) → Doc' ns
  on''' : (target : DOM.EventTarget) (js-event-name : String)
    → (DOM.Event-of js-event-name → IO ⊤)
    → Doc' ns
  append : Doc' ns → Doc' ns → Doc' ns
  empty : Doc' ns
  array : List (Doc' ns) → Doc' ns

Html = Doc' "http://www.w3.org/1999/xhtml"
Svg = Doc' "http://www.w3.org/2000/svg"
Doc = Html -- For backwards compatiblity
