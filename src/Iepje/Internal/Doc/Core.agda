
-- Definition of `Doc`uments

module Iepje.Internal.Doc.Core (e : Set) where

open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)
open import Iepje.Internal.JS.Language.IO using (IO)
open import Iepje.Internal.Renderer.vDOM using (vDOM)
open import Iepje.Internal.Renderer.Cursor using (Cursor)

record EventConsumer : Set where
  constructor mk-EC
  field submit-event : e → IO ⊤

-- Without this 'opaque', yellow warnings would appear any code constructing a nested Doc,
-- since Doc's instance & implicit arguments would be eagerly inserted,
-- then fail to resolve since there are too many instances avaiable.

-- Alternatives to opaque:
-- * Use a data type for Doc
-- * Add a dummy argument
-- * Make {t : String} explicit
-- * Change Agda's default (c.f. Haskell's lazy & shallow instantiation)

opaque
  Doc : String → Set
  Doc ns = 
    {t : String}
    {{cursor : Cursor ns t}}
    {{event-consumer : EventConsumer}}
    → vDOM ns → IO (vDOM ns)

Html = Doc "http://www.w3.org/1999/xhtml"
Svg = Doc "http://www.w3.org/2000/svg"
