
-- Function to update modified DOM in place

module Iepje.Internal.Renderer.Darn where

open import Agda.Builtin.Unit

open import Iepje.Internal.Doc.Core ⊤

open import Iepje.Internal.Renderer.Insert
open import Iepje.Internal.Renderer.Delete

open import Iepje.Internal.Renderer.vDOM
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.Renderer.SubtypingLemmas

open import Iepje.Internal.Utils
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.WebAPIs.DOM

open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality

postulate
  -- This could go in Agda.Builtin.String.Properties, but it is not there
  primStringEqualitySound : ∀ s1 s2 → primStringEquality s1 s2 ≡ true → s1 ≡ s2

private
  _==_ : String → String → Bool
  _==_ = primStringEquality

open Cursor

-- Precondition: cursor at beginning of rendered vDOM
darn : vDOM → Doc → Cursor → IO vDOM
-- These cases may contain focus to preserve
darn (append l₀ r₀) (append l₁ r₁) c = append <$> darn l₀ l₁ c <*> darn r₀ r₁ c
darn (text t₀ e   ) (text t₁     ) c with t₀ == t₁
darn (text t₀ e   ) (text t₁     ) c | true  = do text t₀ e <$ curse (up e) c
darn (d₀          ) (d₁          ) c | false = do delete d₀ c; insert d₁ c
darn (tag  t₀ e d₀) (tag' t₁ f₁  ) c with t₀ == t₁ in eq
darn (tag  t₀ e d₀) (tag' t₁ f₁  ) c | true  = do tag  t₀ e <$> (darn d₀ (f₁ (up {{lem3 t₀ t₁ (primStringEqualitySound t₀ t₁ eq)}} e))
                                                                      =<< init (up {{lem1 t₀}} e))
                                                                      <* curse (up {{lem2 t₀}} e) c
darn (d₀          ) (d₁          ) c | false = do delete d₀ c; insert d₁ c
-- Anything else? Naively delete & re-insert.
darn d₀ d₁ c = do delete d₀ c; insert d₁ c
