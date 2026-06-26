
-- 'Velocity verlet' simulation

-- as inspired by https://d3js.org/d3-force,
-- and documented by
-- https://en.wikipedia.org/wiki/Leapfrog_integration

-- Tests Iepje's support for heavy indirection, and multi-file application

module Iepje.Examples.Verlet.App where

open import Iepje.Examples.Verlet.Numeric.Types
open import Iepje.Examples.Verlet.Numeric.Classes
open import Iepje.Examples.Verlet.Numeric.Instances

open import Iepje.Examples.Verlet.Integrator

open import Agda.Builtin.Equality
open import Iepje.Prelude hiding (_+_; _*_;_/_;_-_;_<_)

-- Simulation code
------------------

module Config where
  -- The system contains a single particle in 2D space,
  -- with floating-point position, velocity, etc.
  Position = 𝔽 ²
  Velocity = 𝔽 ²
  Acceleration = 𝔽 ²
  Time = 𝔽

  -- A force acts on the particle...
  calc-½a : Position → Acceleration

  -- pulling towards the origin with inverse-square strength, F ∝ 1 / x²
  -- (e.g. gravity or electrostatic attraction)
  calc-½a x = - 100000 * x / ∣ x ∣² ^ 1.5

  -- -- pulling towards the origin with constant strength, F ∝ 1
  -- calc-½a x = - 10 * x / ∣ x ∣² ^ 0.5

  -- -- pulling towards the origin with linear strength, F ∝ x
  -- -- (e.g. linear elasticity, from a steel spring)
  -- calc-½a x = - x

  -- -- pulling towards the origin with quadratic strength, F ∝ x²
  -- -- (e.g. non-linear elasticity, from a rubber band)
  -- calc-½a x = - 0.005  * x * ∣ x ∣² ^ 0.5

open Leapfrog (record {Config})
open State

s0 : State
s0 .x =  0.0 , 100.0  -- The particle begins due south of the origin
s0 .v = 30.0 ,   0.0  -- The particle begins travelling due east
s0 .½a = _
s0 .sound = refl

-- GUI code
------------

fps = 10
max-Δt = 0.3   -- If animation lags more than this, pause the simulation

max-trail-segments = 400
comet-radius = 5
trail-radius = 1
sun-radius = 50
hw = 150 -- half of the scene's width, in pixels

disc : Nat → 𝔽 ² → String → Doc ⊤
disc r (cx , cy) color = div do
  style "background" color
  style "border-radius" "50%"
  style "width"  $ show (2 * r) ++ "px"
  style "height" $ show (2 * r) ++ "px"
  style "position" "absolute"
  style "left" $ show (hw + round cx - r) ++ "px"
  style "top"  $ show (hw + round cy - r) ++ "px"

data List1 (A : Set) : Set where
  _∷_ : A → List A → List1 A

take : {A : Set} → Nat → List A → List A
take 0 _ = []
take _ [] = []
take (suc n) (a ∷ as) = a ∷ take n as

view : List1 State → Doc ⊤
view (s ∷ ss) = do
  style "width" (show (2 * hw) ++ "px")
  style "height" (show (2 * hw) ++ "px")
  style "background" "black"
  style "position" "relative"

  concatDocs $ for ss λ old → do
    disc trail-radius (old .x) "blue"
    -- div do style "color" "red"; text (show (round (s .x))) -- Debug

  disc sun-radius (0.0 , 0.0)
    "radial-gradient(circle,white,yellow 25%,orange 30%,transparent 60%)"
  disc comet-radius (s .x)
    "gray"

verlet : IO ⊤
verlet = play "#verlet-app"
  fps
  (s0 ∷ [])
  view
  (λ _ ss → ss)
  (λ where Δt (s ∷ ss) → if max-Δt < Δt then (s ∷ ss) else
              (step Δt s ∷ s ∷ take max-trail-segments ss)
  )
