
-- Translates declarative Html to imperative DOM calls

module Iepje.Internal.Renderer where

open import Iepje.Internal.Html hiding (_>>_)
open import Iepje.Internal.Utils

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
import      Iepje.Internal.JS.WebAPIs.CSSOM as CSSOM
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.IO

open import Agda.Builtin.Unit
open import Agda.Builtin.Sigma

-- Render a doc into an existing DOM Element,
-- and set up `onIO` callbacks.
appendInto : DOM.HTMLElement → Doc ⊤ → IO ⊤

-- Text nodes are created & appended to parent
appendInto pe (text txt) = void do
  hd ← DOM.ownerDocument (up pe)
  t ← DOM.createTextNode hd txt 
  DOM.appendChild (up pe) (up t)

-- Tags are created, recursivey filled, & appended to parent
appendInto pe (tag t d) = void do
  hd ← DOM.ownerDocument (up pe)
  el ← up {{DOM.Element-of t .snd}}
    <$> DOM.createElement hd t
  appendInto (up el) d
  DOM.appendChild (up pe) (up el)

-- Attributes are set on parent (via DOM API)
appendInto pe (attr k v) = void do
  DOM.setAttribute (up pe) k v

-- Styles are appended to parent's style (via CSSOM API)
appendInto pe (style k v) = void do
  sd ← DOM.get-style (up pe)
  CSSOM.setProperty sd k v

-- Callbacks are registered on parent
appendInto pe (onIO js-event-name handler) = void do
  DOM.addEventListener (up pe) js-event-name handler

-- Concatenated docs are processed in order
appendInto pe (append d1 d2) = do
  appendInto pe d1
  appendInto pe d2

-- Empty documents are ignored
appendInto pe empty = pure tt
