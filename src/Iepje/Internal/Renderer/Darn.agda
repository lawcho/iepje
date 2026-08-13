
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
open import Iepje.Internal.Renderer.Cursor

open import Iepje.Internal.Utils
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.WebAPIs.DOM

open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality
open import Agda.Builtin.Nat renaming (_==_ to _==ₙ_)

postulate
  -- This could go in Agda.Builtin.String.Properties, but it is not there
  primStringEqualitySound : ∀ {s1 s2} → primStringEquality s1 s2 ≡ true → s1 ≡ s2

private
  _==ₛ_ : String → String → Bool
  _==ₛ_ = primStringEquality

  lem' : ∀{ns₀ ns₁ t₀ t₁}
      → ns₀ ≡ ns₁
      → t₀ ≡ t₁
      → (ElementNS-of ns₁ t₁ → Doc' ns₁) → ElementNS-of ns₀ t₀ → Doc' ns₀
  lem' refl refl z = z

  lem : ∀{ns₀ ns₁ t₀ t₁}
      → ns₀ ==ₛ ns₁ ≡ true
      → t₀ ==ₛ t₁ ≡ true
      → (ElementNS-of ns₁ t₁ → Doc' ns₁) → ElementNS-of ns₀ t₀ → Doc' ns₀
  lem x y = lem' (primStringEqualitySound x) (primStringEqualitySound y)

open Cursor

-- Precondition: cursor at beginning of rendered vDOM
darn : ∀{ns t} → Cursor ns t → vDOM ns → Doc' ns → IO (vDOM ns)
{-# TERMINATING #-}
-- These cases may contain focus to preserve
darn c (with-parent d₀) (with-parent f₁) =
  with-parent <$> darn c d₀ (f₁ (up (c .parent)))
darn c (with-document d₀) (with-document f₁) =
  with-document <$> do doc ← document; darn c d₀ (f₁ (up doc))
darn c (with-submit-event d₀) (with-submit-event f₁) =
  with-submit-event <$> darn c d₀ (f₁ submit-event)
darn c (append l₀ r₀) (append l₁ r₁) = append <$> darn c l₀ l₁ <*> darn c r₀ r₁
darn c (text t₀ e   ) (text t₁     ) with t₀ ==ₛ t₁
darn c (text t₀ e   ) (text t₁     ) | true  = do text t₀ e <$ curse (up e) c
darn c (d₀          ) (d₁          ) | false = do delete c d₀; insert c d₁
darn c (tag ns₀ t₀ e d₀) (ns-tag' ns₁ t₁ f₁  ) with ns₀ ==ₛ ns₁ in eqₙₛ | t₀ ==ₛ t₁ in eqₜ
darn c (tag ns₀ t₀ e d₀) (ns-tag' ns₁ t₁ f₁  ) | true | true = do
  c' ← init e
  tag  ns₀ t₀ e <$> darn c' d₀ (lem eqₙₛ eqₜ f₁ e) <* curse (up e) c
darn c (d₀          ) (d₁          ) | _ | _ = do delete c d₀; insert c d₁
darn c (array ds₀) (array ds₁) with length ds₀ ==ₙ length ds₁ in eq
darn c (array ds₀) (array ds₁) | true  = array <$> sequenceA (zipWith (darn c) ds₀ ds₁ (==-to-≡ eq))
darn c (d₀       ) (d₁       ) | false = do delete c d₀; insert c d₁
-- Anything else? Naively delete & re-insert.
darn c d₀ d₁ = do delete c d₀; insert c d₁
