
-- Function to update modified DOM in place

open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit

module Iepje.Internal.Renderer.Darn
  {event : Set}
  (submit-event : event → IO ⊤)
  where

open import Iepje.Internal.Doc.Core event

open import Iepje.Internal.Renderer.Insert submit-event
open import Iepje.Internal.Renderer.Delete

open import Iepje.Internal.Renderer.vDOM
open import Iepje.Internal.Renderer.Cursor as C using (Cursor)

open import Iepje.Internal.Utils
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.WebAPIs.DOM

open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality
open import Agda.Builtin.Maybe
open import Agda.Builtin.Nat renaming (_==_ to _==ₙ_)

open Cursor {{...}}

-- Precondition: cursor at beginning of rendered vDOM
darn : ∀{ns t} → {{Cursor ns t}} → vDOM ns → Doc ns → IO (vDOM ns)
{-# TERMINATING #-}
-- These cases may contain focus to preserve
darn (with-parent d₀) (with-parent f₁) =
  with-parent <$> darn d₀ (f₁ (up parent))
darn (with-document d₀) (with-document f₁) =
  with-document <$> do doc ← document; darn d₀ (f₁ (up doc))
darn (with-submit-event d₀) (with-submit-event f₁) =
  with-submit-event <$> darn d₀ (f₁ submit-event)
darn (append l₀ r₀) (append l₁ r₁) = append <$> darn l₀ l₁ <*> darn r₀ r₁
darn (text t₀ e   ) (text t₁     ) with t₀ ==ₛ t₁
darn (text t₀ e   ) (text t₁     ) | true  = do text t₀ <$> C.curse e
darn (d₀          ) (d₁          ) | false = do delete d₀; insert d₁
darn (tag ns₀ t₀ e d₀) (ns-tag' ns₁ t₁ f₁  ) with ns₀ ≟ₛ ns₁ | t₀ ≟ₛ t₁
darn (tag ns₀ t₀ e d₀) (ns-tag' ns₁ t₁ f₁  ) | just refl | just refl = do
  c' ← C.init e
  tag  ns₀ t₀ <$> C.curse e <*> darn {{c'}} d₀ (f₁ e)
darn (d₀          ) (d₁          ) | _ | _ = do delete d₀; insert d₁
darn (array ds₀) (array ds₁) with length ds₀ ≟ₙ length ds₁
darn (array ds₀) (array ds₁) | just p  = array <$> sequenceA (zipWith darn ds₀ ds₁ p)
darn (d₀       ) (d₁       ) | nothing = do delete d₀; insert d₁
-- Anything else? Naively delete & re-insert.
darn d₀ d₁ = do delete d₀; insert d₁
