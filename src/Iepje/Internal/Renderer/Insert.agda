
-- Function to render new vDOM into the DOM

open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit

module Iepje.Internal.Renderer.Insert
  {event : Set}
  (submit-event : event → IO ⊤)
  where


open import Iepje.Internal.Doc.Core event
open import Iepje.Internal.Renderer.vDOM
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.Utils

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.SubTyping

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

open Cursor

-- Postcondition: cursor moved after the inserted nodes
insert : ∀{ns t} → Cursor ns t → Doc' ns → IO (vDOM ns)
{-# TERMINATING #-}
insert c (text  t) = do e ← DOM.createTextNode (c .doc) t; insert-after c (up e); text t e <$ pure tt
insert c (ns-tag' ns t f) = do e ← DOM.createElementNS (c .doc) ns t; insert-after c (up e); c' ← init e; tag ns t e <$> insert c' (f e)
insert c (on'''  t n k) = on''' t n <$> do
    l ← DOM.mk-event-listener k
    DOM.addEventListener t n l
    pure l
insert c (with-parent f) = with-parent <$> insert c (f (up (c .parent)))
insert c (with-document f) = do d ← DOM.document; with-document <$> insert c (f (up d))
insert c (with-submit-event f) = insert c (f submit-event)
insert c (attr  k v) = attr  k v <$ pure tt -- Hack: ignore attrs, always reapply in future pass
insert c (style k v) = style k v <$ pure tt -- Hack: ignore style, always reapply in future pass
insert c empty       = empty     <$ pure tt
insert c (append d₀ d₁) = append <$> insert c d₀ <*> insert c d₁
insert c (array ds) = array <$> sequenceA (map (insert c) ds)
