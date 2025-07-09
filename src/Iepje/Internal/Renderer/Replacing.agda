
-- Naive rendering algorithm that creates new DOM elements

module Iepje.Internal.Renderer.Replacing where

open import Agda.Builtin.Unit

open import Iepje.Internal.Doc.Core ⊤
open import Iepje.Internal.Renderer.vDOM ⊤

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
import      Iepje.Internal.JS.WebAPIs.CSSOM as CSSOM

open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.GlobalObjects using (null)
open import Iepje.Internal.JS.Language.MutableReferences as Ref

open import Iepje.Internal.Utils
open import Agda.Builtin.Maybe
open import Agda.Builtin.Sigma

Parent = DOM.HTMLElement

-- Clear all vDOM-tracked data from an HTMLElement
clear : Parent → vDOM → IO ⊤
clear p (tag e _ _) = void $ DOM.removeChild (up p) (up e)
clear p (text e _)  = void $ DOM.removeChild (up p) (up e)
clear p (style k _) = void $ do sd ← DOM.get-style (up p); CSSOM.removeProperty sd k
clear p (attr k _)  = void $ DOM.removeAttribute (up p) k
clear p (onIO n k) = void $ DOM.removeEventListener (up p) n k
clear p (doc-onIO n k) = void $ do
  hd ← DOM.get-ownerDocument (up p)
  DOM.removeEventListener (up hd) n k
clear p (append d1 d2) = do clear p d1 ; clear p d2
clear p empty = pure tt

-- Data type tracking a position inside a HTMLElement
data Cursor : Set where
  at-beginning : Cursor
  after : DOM.Node → Cursor

-- Render a Doc into a (vDOM-tracked) HTMLElement,
--  * after a given point in the HTML element
--  * creating new nodes
--  * ignoring styles & attributes
--  * and returning a new vDOM tracker
insert
  : Parent
  → Ref Cursor
  → Doc
  → IO vDOM
insert parent cursor = go where

  -- Advance the cursor
  curse : {A : Set} → A → {{A extends* DOM.Node}} → IO A
  curse e = do
    Ref.set cursor (after $ up e)
    pure e

  -- Insert a node and andvacne the cursor
  insert-Node : ∀{A} → {{A extends* DOM.Node}} → A → IO A
  insert-Node n = do
    right-sib ← Ref.get cursor >>= λ where
      (after left-sib) → DOM.get-nextSibling left-sib
      at-beginning     → DOM.get-firstChild (up parent)
    DOM.insertBefore (up parent) (up n) right-sib
    curse n

  -- Main traversal function
  go : Doc → IO vDOM
  go (tag t cd) = do
    hd ← DOM.get-ownerDocument (up parent)
    el ← DOM.createElement hd t
    ccursor ← Ref.new at-beginning
    vDOM.tag $$ insert-Node el $$ t $$ insert el ccursor cd

  go (text txt) = do
    hd ← DOM.get-ownerDocument (up parent)
    el ← DOM.createTextNode hd txt
    vDOM.text $$ insert-Node el $$ txt

  go (onIO n k) = do
    l ← DOM.mk-event-listener k
    DOM.addEventListener (up parent) n l
    pure $ onIO n l

  go (doc-onIO n k) = do
    hd ← DOM.get-ownerDocument (up parent)
    l ← DOM.mk-event-listener k
    DOM.addEventListener (up hd) n l
    pure $ doc-onIO n l

  go (append dL dR) = vDOM.append $$ go dL $$ go dR
  go (style k v)    = vDOM.style $$ k $$ v -- Style ignored
  go (attr k v)     = vDOM.attr  $$ k $$ v -- Attr ignored
  go empty          = pure empty

-- Apply styles and attributes tracked in a vDOM
apply-styles-and-attrs : Parent → vDOM → IO ⊤
apply-styles-and-attrs = go where
  go : Parent → vDOM → IO ⊤
  go p (style k v) = void do
    sd ← DOM.get-style (up p)
    CSSOM.setProperty sd k v
  go p (attr k v)     = void $ DOM.setAttribute (up p) k v
  go p (append dL dR) = go p dL >> go p dR
  go p (tag e _ d)    = go e d
  go _ _              = pure tt
