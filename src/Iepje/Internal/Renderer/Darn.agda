
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

postulate
  -- This could go in Agda.Builtin.String.Properties, but it is not there
  primStringEqualitySound : ∀ {s1 s2} → primStringEquality s1 s2 ≡ true → s1 ≡ s2

private
  _==_ : String → String → Bool
  _==_ = primStringEquality

  lem' : ∀{ns₀ ns₁ t₀ t₁}
      → ns₀ ≡ ns₁
      → t₀ ≡ t₁
      → (ElementNS-of ns₁ t₁ → Doc' ns₁) → ElementNS-of ns₀ t₀ → Doc' ns₀
  lem' refl refl z = z

  lem : ∀{ns₀ ns₁ t₀ t₁}
      → ns₀ == ns₁ ≡ true
      → t₀ == t₁ ≡ true
      → (ElementNS-of ns₁ t₁ → Doc' ns₁) → ElementNS-of ns₀ t₀ → Doc' ns₀
  lem x y = lem' (primStringEqualitySound x) (primStringEqualitySound y)

open Cursor

-- Precondition: cursor at beginning of rendered vDOM
darn : ∀{ns t} → vDOM ns → Doc' ns → Cursor ns t → IO (vDOM ns)
-- These cases may contain focus to preserve
darn (with-parent d₀) (with-parent f₁) c =
  with-parent <$> darn d₀ (f₁ (up (c .parent))) c
darn (with-document d₀) (with-document f₁) c =
  with-document <$> do doc ← document; darn d₀ (f₁ (up doc)) c
darn (with-submit-event d₀) (with-submit-event f₁) c =
  with-submit-event <$> darn d₀ (f₁ submit-event) c
darn (append l₀ r₀) (append l₁ r₁) c = append <$> darn l₀ l₁ c <*> darn r₀ r₁ c
darn (text t₀ e   ) (text t₁     ) c with t₀ == t₁
darn (text t₀ e   ) (text t₁     ) c | true  = do text t₀ e <$ curse (up e) c
darn (d₀          ) (d₁          ) c | false = do delete d₀ c; insert d₁ c
darn (tag ns₀ t₀ e d₀) (ns-tag' ns₁ t₁ f₁  ) c with ns₀ == ns₁ in eqₙₛ | t₀ == t₁ in eqₜ
darn (tag ns₀ t₀ e d₀) (ns-tag' ns₁ t₁ f₁  ) c | true | true = do
  tag  ns₀ t₀ e <$> (darn d₀ (lem eqₙₛ eqₜ f₁ e) =<< init e) <* curse (up e) c
darn (d₀          ) (d₁          ) c | _ | _ = do delete d₀ c; insert d₁ c
-- Anything else? Naively delete & re-insert.
darn d₀ d₁ c = do delete d₀ c; insert d₁ c
