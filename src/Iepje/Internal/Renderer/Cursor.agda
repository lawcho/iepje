-- 'Cursor' type used in various DOM traverals

module Iepje.Internal.Renderer.Cursor where

open import Iepje.Internal.JS.WebAPIs.DOM as DOM using ()
open import Iepje.Internal.JS.WebAPIs.CSSOM as CSSOM using ()
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.PrimitiveTypes using (null)
open import Iepje.Internal.JS.Language.MutableReferences as Ref
open import Iepje.Internal.JS.Language.MutableDictionaries as Dict
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.FromUnion
open import Iepje.Internal.JS.Language.Undefined
open import Iepje.Internal.Doc.Has-style

open import Iepje.Internal.Utils

open import Agda.Builtin.Unit
open import Agda.Builtin.Maybe
open import Agda.Builtin.String

-- A cursor points to a particular position in vDOM & Browser DOM
record Cursor (namespace tag : String) : Set where
  constructor cursor
  field
    doc : DOM.Document                      -- DOM Document containing the cursor
    parent : DOM.ElementNS-of namespace tag -- DOM Element above the cursor
    prev-node : Ref (Maybe DOM.Node)        -- DOM Node closest before the cursor
    prev-styles : Dict String               -- vDOM style values closest before the cursor 
    prev-attrs  : Dict String               -- vDOM attr values closest before the cursor

  -- Invariant: `prev-node` points to a child of `parent` in the DOM
  -- Invariant: `prev-node` points to a node contained in `doc`
  -- Invariant: `parent` is contained in `doc`

open Cursor {{...}}
open Has-style {{...}}

-- Get the DOM Node after the cursor
next-node : ∀{ns t} → {{Cursor ns t}} → IO (DOM.Node ∪ null)
next-node = Ref.get prev-node >>= λ where
  (just l) → DOM.get-nextSibling (up l)
  nothing → DOM.get-firstChild (up parent)

-- Make a new cursor pointing just before the first child of a DOM Element
init : ∀{ns t} → DOM.ElementNS-of ns t → IO (Cursor ns t)
init el = cursor
            <$> DOM.get-ownerDocument (up el)
            <*> pure el
            <*> Ref.new nothing
            <*> Dict.empty
            <*> Dict.empty

-- Move the cursor after a DOM Node
-- returns the same node passed in, allowing inline use
-- Precondition: node under cursor's parent in DOM
curse : ∀{ns t N} → {{Cursor ns t}} → {{N extends* DOM.Node}} → N → IO N
curse n = do
  Ref.set prev-node (just (up n))
  pure n

-- Insert a node after the cursor, and move the cursor after it
-- returns the same node passed in, allowing inline use
insert : ∀{ns t N} → {{Cursor ns t}} → {{N extends* DOM.Node}} → N → IO N
insert n = do
  DOM.insertBefore (up parent) (up n) =<< next-node
  curse n

-- Apply the stored style to the parent element
restore-style : ∀{ns t} → {{Cursor ns t}} → {{Has-style ns}} → String → IO ⊤
restore-style k = do
  css ← get-style parent
  action ← Dict.get k
    (CSSOM.setProperty css k) -- Overwrite k's value
    (undefined <$ CSSOM.removeProperty css k) -- Delete k's value
    prev-styles
  action
  pure tt

-- Record a style in the cache (& apply it)
-- returns the new value, allowing inline use
curse-style : ∀{ns t} → {{Cursor ns t}} → {{Has-style ns}} → String → String → IO String
curse-style {{c}} k v = do
  Dict.set k v (c .prev-styles)
  css ← get-style parent
  CSSOM.setProperty css k v
  pure v

-- Apply stored attr to the parent element
restore-attr : ∀{ns t} → {{Cursor ns t}} → String → IO ⊤
restore-attr k = do
  action ← Dict.get k
    (DOM.setAttribute (up parent) k) -- Overwrite k's value
    (undefined <$ DOM.removeAttribute (up parent) k) -- Delete k's value
    prev-attrs
  action
  pure tt

-- Record an attribute in the cache (& apply it)
-- returns the new value, allowing inline use
curse-attr : ∀{ns t} → {{Cursor ns t}} → String → String → IO String
curse-attr {{c}} k v = do
  Dict.set k v (c .prev-attrs)
  DOM.setAttribute (up parent) k v
  pure v

