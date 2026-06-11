
-- Typeclass for namespaces supporting a "style" atrtribute on their elements

module Iepje.Internal.Doc.Has-style where

open import Agda.Builtin.String
open import Iepje.Internal.JS.WebAPIs.DOM as DOM using (ElementNS-of)
open import Iepje.Internal.JS.WebAPIs.CSSOM using (CSSStyleDeclaration)
open import Iepje.Internal.JS.Language.IO using (IO)
open import Iepje.Internal.JS.Language.SubTyping

record Has-style (namespace : String) : Set where
  field get-style : ∀{tag} → ElementNS-of namespace tag → IO CSSStyleDeclaration
open Has-style

module Instances where instance
  Has-style-html : Has-style "http://www.w3.org/1999/xhtml"
  Has-style-html .get-style e = DOM.HTMLElement-methods.get-style (up e)

  Has-style-svg : Has-style "http://www.w3.org/2000/svg"
  Has-style-svg .get-style e = DOM.SVGElement-methods.get-style (up e)
