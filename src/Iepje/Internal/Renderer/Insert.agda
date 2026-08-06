
-- Function to render new vDOM into the DOM

module Iepje.Internal.Renderer.Insert where

open import Agda.Builtin.Unit

open import Iepje.Internal.Doc.Core ⊤
open import Iepje.Internal.Renderer.vDOM
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.SubTyping

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

open Cursor

private
  listen : DOM.EventTarget → (n : String) → (DOM.Event-of n → IO ⊤) → IO (DOM.event-listener n)
  listen t n k = do
    l ← DOM.mk-event-listener k
    DOM.addEventListener t n l
    pure l

-- Postcondition: cursor moved after the inserted nodes
insert : ∀{ns t} → Doc' ns → Cursor ns t → IO (vDOM ns)
insert (text  t) c = do e ← DOM.createTextNode (c .doc) t; insert-after (up e) c; text t e <$ pure tt
insert (ns-tag' ns t f) c = do e ← DOM.createElementNS (c .doc) ns t; insert-after (up e) c; tag ns t e <$> (insert (f e) =<< init e)
insert (onIO'  t n k) c = onIO t n <$> listen t n k
insert (with-parent f) c = with-parent <$> insert (f (up (c .parent))) c
insert (with-document f) c = do d ← DOM.document; with-document <$> insert (f (up d)) c
insert (attr  k v) _ = attr  k v <$ pure tt -- Hack: ignore attrs, always reapply in future pass
insert (style k v) _ = style k v <$ pure tt -- Hack: ignore style, always reapply in future pass
insert empty       _ = empty     <$ pure tt
insert (append d₀ d₁) c = append <$> insert d₀ c <*> insert d₁ c
