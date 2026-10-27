
-- Function to render new vDOM into the DOM

open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit

module Iepje.Internal.Renderer.Insert
  {event : Set}
  (submit-event : event → IO ⊤)
  where


open import Iepje.Internal.Doc.Core event
open import Iepje.Internal.Renderer.vDOM
open import Iepje.Internal.Renderer.Cursor as C using (Cursor) 
open import Iepje.Internal.Utils

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.SubTyping

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

open Cursor {{...}}

-- Postcondition: cursor moved after the inserted nodes
insert : ∀{ns t} → {{Cursor ns t}} → Doc ns → IO (vDOM ns)
{-# TERMINATING #-}
insert (text  t) = do e ← DOM.createTextNode doc t; text t <$> C.insert e
insert (ns-tag' ns t f) = do
  e ← DOM.createElementNS doc ns t
  c' ← C.init e
  tag ns t <$> C.insert e <*> insert {{c'}} (f e)
insert (on'''  t n k) = on''' t n <$> do
    l ← DOM.mk-event-listener k
    DOM.addEventListener t n l
    pure l
insert (with-parent f) = with-parent <$> insert (f (up parent))
insert (with-document f) = do d ← DOM.document; with-document <$> insert (f d)
insert (with-submit-event f) = insert (f submit-event)
insert (attr  k v) = attr  k v <$ pure tt -- Hack: ignore attrs, always reapply in future pass
insert (style k v) = style k v <$ pure tt -- Hack: ignore style, always reapply in future pass
insert empty       = empty     <$ pure tt
insert (append d₀ d₁) = append <$> insert d₀ <*> insert d₁
insert (array ds) = array <$> sequenceA (map insert ds)
