
-- Arithmetic quiz game

-- Demonstrates text input (without page reloads),
-- focus retention despite disappearing DOM,
-- and calling Element sub-class methods

module Iepje.Examples.Quiz where

open import Iepje.Prelude hiding (for'; map)

module Fin where
  data Fin : Nat → Set where
    zero : ∀{n} → Fin (suc n)
    suc : ∀{n} → Fin n → Fin (suc n)

  to-Nat : ∀{n} → Fin n → Nat
  to-Nat zero = zero
  to-Nat (suc f) = suc (to-Nat f)
open Fin using (Fin; zero; suc)

module Vec where

  data Vec (A : Set) : Nat → Set where
    [] : Vec A 0
    _∷_ : ∀{n} → A → Vec A n → Vec A (suc n)

  infixr 5 _∷_

  for' : ∀{A B n} → Vec A n → (Fin n → A → B) → Vec B n
  for' [] _ = []
  for' (x ∷ v) f = f zero x ∷ for' v (f ∘ suc)

  to-List : ∀{A n} → Vec A n → List A
  to-List [] = []
  to-List (x ∷ v) = x ∷ to-List v

  map :  ∀{A B n} → (A → B) → Vec A n → Vec B n
  map f v = for' v λ _ → f

  all : ∀{A n} → (A → Bool) → Vec A n → Bool
  all p v = foldr _&&_ true (to-List (map p v))

  update : ∀{A n} → (A → A) → Fin n → Vec A n → Vec A n
  update f zero (a ∷ as) = f a ∷ as
  update f (suc i) (a ∷ as) = a ∷ update f i as

open Vec using (Vec; []; _∷_)

Milliseconds = Nat

record Exercise : Set where
  field question         : String
  field expected-answer  : String
  field current-answer   : String 
  field millis-remaining : Milliseconds
  field millis-total     : Milliseconds
open Exercise

data Msg (n : Nat) : Set where
  submit : Fin n → Msg n
  change : Fin n → String → Msg n

view : ∀{n} → Vec Exercise n → Doc (Msg n)
view v with Vec.all (λ ex → ex .millis-remaining == 0) v
-- View results of quiz
... | true = do
  tag "table" do
    style "width" "min-content"
    tag "tr" do
      tag "th" $ text "Question"
      tag "th" $ text "Your answer"
      tag "th" $ text "Correct answer"
    concatDocs $ Vec.to-List $ Vec.for' v λ _ ex → do
      tag "tr" do
        tag "td" $ text $ ex .question
        tag "td" $ text $ ex .current-answer
        tag "td" $ text $ ex .expected-answer
  tag "hr" empty
  text "Reload the page to try again"
-- View input fields for quiz
... | false = do
    style "display" "grid"
    style "grid-auto-rows" "1fr"
    style "grid-template-columns" "auto auto"
    concatDocs $ Vec.to-List $ Vec.for' v λ i ex →
      if ex .millis-remaining == 0 then empty else do
      let percent = primShowNat ((100 * ex .millis-remaining) / ex .millis-total) ++ "%"
      tag "form" do
        style "grid-row" $ primShowNat (1 + Fin.to-Nat i)
        style "display" "grid"
        style "grid-template-columns" "subgrid"
        style "grid-column" "1 / 3"
        tag "label" do
          style "display" "grid"
          style "grid-template-columns" "subgrid"
          style "grid-column" "1 / 3"
          span do
            text $ ex .question
            style "grid-column" "1 / 2"
          tag' "input" λ e → do
            attr "type" "text"
            style "grid-column" "2 / 3"
            style "background-image" ("
                linear-gradient(
                  to right,
                  lightgrey ,
                  lightgrey "++ percent ++",
                  transparent "++ percent ++",
                  transparent
                  )")
            onIO "change" λ _ → IO.do
              s ← DOM.get-value e
              IO.pure $ change i s
        onIO "submit" λ e → IO.do
          DOM.preventDefault e  -- don't reload the webpage upon submission
          IO.pure $ submit i

update : ∀{n} → Msg n → Vec Exercise n → Vec Exercise n
update (submit i) = Vec.update (λ ex → record ex {millis-remaining = 0}) i
update (change i s) = Vec.update (λ ex → record ex {current-answer = s}) i

tick : ∀{n} → Float → Vec Exercise n → Vec Exercise n
tick Δt with primFloatRound $ primFloatTimes Δt 1000.0
tick _ | (just (pos millis)) = Vec.map (λ ex → record ex {millis-remaining = ex .millis-remaining - millis})
tick _ | _ = λ exs → exs

mul-ex : Nat → Nat → Exercise
mul-ex m n .question = primShowNat m ++ " * " ++ primShowNat n ++ " = "
mul-ex m n .expected-answer = primShowNat (m * n)
mul-ex m n .current-answer  = ""
mul-ex m n .millis-remaining = 1000 * (2 + m + n)
mul-ex m n .millis-total     = 26000

exercises : Vec Exercise _
exercises =
  mul-ex 3 7 ∷
  mul-ex 11 7 ∷
  mul-ex 1 3 ∷
  mul-ex 2 5 ∷
  mul-ex 8 12 ∷
  []

import Agda.Builtin.Float

quiz : IO ⊤
quiz = play "#quiz-app" 20 exercises view update tick
