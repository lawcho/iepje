
-- Function to delete a vDOM from the DOM

module Iepje.Internal.Renderer.Delete where

open import Iepje.Internal.Renderer.vDOM


import      Iepje.Internal.JS.WebAPIs.DOM as DOM
import      Iepje.Internal.JS.WebAPIs.CSSOM as CSSOM
open import Iepje.Internal.Doc.Has-style

open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences as Ref
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.Utils

open import Agda.Builtin.Unit
open import Agda.Builtin.Maybe
open import Agda.Builtin.Sigma

open Has-style {{...}}
open Cursor

-- Precondition: cursor has correct parent
-- Postcondition: cursor unmoved
delete : ∀{ns t} → Cursor ns t → vDOM ns → IO ⊤
{-# TERMINATING #-}
delete c (text   t e  ) = void $ do DOM.removeChild (up (c .parent)) (up e)
delete c (tag ns t e d) = void $ do DOM.removeChild (up (c .parent)) (up e); c' ← init e; delete c' d
delete c (on'''   t n k) = void $ do DOM.removeEventListener t n k
delete c (with-parent d) = void $ do delete c d
delete c (with-document d) = void $ do delete c d
delete c (with-submit-event d) = void $ do delete c d
delete c (attr  k v)    = void $ do DOM.removeAttribute (up (c .parent)) k
delete c (style k v)    = void $ do sd ← get-style (c .parent); CSSOM.removeProperty sd k
delete c empty          = void $ pure tt
delete c (append d₀ d₁) = void $ do delete c d₀; delete c d₁
delete c (array ds) = void $ sequenceA $ map (delete c) ds
