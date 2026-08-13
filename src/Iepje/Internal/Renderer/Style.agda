
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
re-style : ∀{ns t} → Cursor ns t → vDOM ns → IO ⊤
{-# TERMINATING #-}
re-style c (attr  k v)    = do DOM.setAttribute (up (c .parent)) k v; pure tt
re-style c (style k v)    = do css ← get-style (c .parent); CSSOM.setProperty css k v; pure tt
re-style c (tag ns t e d) = do c' ← init e; re-style c' d
re-style c (append d₀ d₁) = do re-style c d₀ ; re-style c d₁
re-style c (with-parent d) = do re-style c d
re-style c (with-document d) = do re-style c d
re-style c (with-submit-event d) = do re-style c d
re-style c (text _ _)    = pure tt
re-style c (on''' _ _ _) = pure tt
re-style c empty         = pure tt
re-style c (array ds)    = do sequenceA $ map (re-style c) ds; pure tt