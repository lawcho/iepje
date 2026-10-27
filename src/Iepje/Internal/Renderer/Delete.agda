
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
open Cursor {{...}}

-- Precondition: cursor has correct parent
-- Postcondition: cursor unmoved
delete : ∀{ns t} → {{Cursor ns t}} → vDOM ns → IO ⊤
{-# TERMINATING #-}
delete (text   t e  ) = void $ do DOM.removeChild (up parent) (up e)
delete (tag ns t e d) = void $ do DOM.removeChild (up parent) (up e); c' ← init e; delete {{c'}} d
delete (on'''   t n k) = void $ do DOM.removeEventListener t n k
delete (with-parent d) = void $ do delete d
delete (with-document d) = void $ do delete d
delete (with-submit-event d) = void $ do delete d
delete (attr  k v)    = void $ do DOM.removeAttribute (up parent) k
delete (style k v)    = void $ do sd ← get-style parent; CSSOM.removeProperty sd k
delete empty          = void $ pure tt
delete (append d₀ d₁) = void $ do delete d₀; delete d₁
delete (array ds) = void $ sequenceA $ map delete ds
