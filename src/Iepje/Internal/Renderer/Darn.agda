
-- Function to update modified DOM in place

module Iepje.Internal.Renderer.Darn where

open import Iepje.Internal.JS.WebAPIs.DOM as DOM using ()
open import Iepje.Internal.Doc.Core using (Doc; mk-EC)
open import Iepje.Internal.Renderer.Cursor using (Cursor; init)
open import Iepje.Internal.Renderer.vDOM using (vDOM)

open import Iepje.Internal.JS.Language.IO using (IO; _>>=_; pure)
open import Iepje.Internal.Utils

open import Agda.Builtin.Unit


opaque
  unfolding Doc

  -- Precondition: cursor at beginning of old vDOM
  -- Postcondition: cursor at end of new vDOM
  darn : ∀{ns e t}
    → DOM.ElementNS-of ns t
    → (e → IO ⊤)
    → Doc e ns
    → vDOM ns
    → IO (vDOM ns)
  darn parent submit-event d v =
    init parent >>=ᵢ
    mk-EC submit-event &ᵢ
    d v
