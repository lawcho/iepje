
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
delete : ∀{ns t} → vDOM ns → Cursor ns t → IO ⊤
delete (text   t e  ) c = void $ do DOM.removeChild (up (c .parent)) (up e)
delete (tag ns t e d) c = void $ do DOM.removeChild (up (c .parent)) (up e); delete d =<< init e
delete (on'''   t n k) c = void $ do DOM.removeEventListener t n k
delete (with-parent d) c = void $ do delete d c
delete (with-document d) c = void $ do delete d c
delete (with-submit-event d) c = void $ do delete d c
delete (attr  k v)    c = void $ do DOM.removeAttribute (up (c .parent)) k
delete (style k v)    c = void $ do sd ← get-style (c .parent); CSSOM.removeProperty sd k
delete empty          _ = void $ pure tt
delete (append d₀ d₁) c = void $ do delete d₀ c; delete d₁ c
