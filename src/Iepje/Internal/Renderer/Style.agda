
-- Function to re-apply styles from a vDOM

module Iepje.Internal.Renderer.Style where

open import Iepje.Internal.Renderer.vDOM

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
import      Iepje.Internal.JS.WebAPIs.CSSOM as CSSOM
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences as Ref
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.Utils
open import Iepje.Internal.Doc.Has-style

open Has-style {{...}}

open import Agda.Builtin.Unit
open import Agda.Builtin.Maybe

open Cursor

-- Pre-condition: cursor has correct parent
-- Postcondition: cursor unmoved
re-style : ∀{ns t} → vDOM ns → Cursor ns t → IO ⊤
re-style (attr  k v)    c = do DOM.setAttribute (up (c .parent)) k v; pure tt
re-style (style k v)    c = do css ← get-style (c .parent); CSSOM.setProperty css k v; pure tt
re-style (tag ns t e d) c = do re-style d =<< init e
re-style (append d₀ d₁) c = do re-style d₀ c ; re-style d₁ c
re-style (with-parent d) c = do re-style d c
re-style (with-document d) c = do re-style d c
re-style (with-submit-event d) c = do re-style d c
re-style (text _ _)    c = pure tt
re-style (on''' _ _ _) c = pure tt
re-style empty         c = pure tt
