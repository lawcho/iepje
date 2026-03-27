
-- Sup-typing lemmas used thruoghout the renderer

module Iepje.Internal.Renderer.SubtypingLemmas where


open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.WebAPIs.DOM

open import Agda.Builtin.Sigma
open import Agda.Builtin.Equality

lem1 : ∀ t → Element-of t .fst extends* HTMLElement
lem1 t = Element-of t .snd

lem2 : ∀ t → Element-of t .fst extends* Node
lem2 t = ∘-extends* (lem1 t) (extends-cons {{sup-HTMLElement}})

lem3 : ∀ t₀ t₁ → (t₀ ≡ t₁) → Element-of t₀ .fst extends* Element-of t₁ .fst
lem3 _ _ refl = extends-refl

