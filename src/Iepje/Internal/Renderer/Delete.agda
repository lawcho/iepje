
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
delete (text   t e  ) = DOM.removeChild (up parent) (up e) $> tt
delete (tag ns t e d) = DOM.removeChild (up parent) (up e) >> (delete d =<<ᵢ init e)
delete (on'''   t n k) = DOM.removeEventListener t n k $> tt
delete (attr  k _)    = restore-attr k
delete (style k _)    = restore-style k
delete empty          = pure tt
delete (append d₀ d₁) = delete d₀ >> delete d₁
delete (array ds) = sequenceA (map delete ds) $> tt
