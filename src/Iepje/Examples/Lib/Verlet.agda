
-- Definitions for performing a single step of verlet integration
-- as documented at 
-- https://en.wikipedia.org/wiki/Leapfrog_integration
module Iepje.Examples.Lib.Verlet where

open import Iepje.Examples.Lib.Numeric.Classes
open import Agda.Builtin.Equality

record VerletConfig : Set₁ where
  eta-equality
  field Position : Set
  field Velocity : Set
  field Acceleration : Set
  field Time : Set
  field calc-½a : Position → Acceleration
  -- These field are marked with 'instance' for use inside the 'Stepper' module,
  -- and marked with {{}} for easier construction of VerletConfig downstream
  field instance {{h+-VV}} : Has-+ Velocity Velocity Velocity
  field instance {{h*-AT}} : Has-* Acceleration Time Velocity
  field instance {{h+-PP}} : Has-+ Position Position Position
  field instance {{h*-VT}} : Has-* Velocity Time Position

module Leapfrog (config : VerletConfig) where
  open VerletConfig config

  record State : Set where
    field v : Velocity
    field x : Position
    field ½a : Acceleration
    field sound : ½a ≡ calc-½a x
  open State

  step : Time → State → State
  step Δt i = record {v = vᵢ₊₁; x = xᵢ₊₁; ½a = ½aᵢ₊₁; sound = refl} where

    vᵢ₊ₕ = v i + ½a i * Δt  -- kick
    xᵢ₊₁ = x i + vᵢ₊ₕ * Δt  -- drift
    ½aᵢ₊₁ = calc-½a xᵢ₊₁    -- (adjust strength of future kicks)
    vᵢ₊₁ = vᵢ₊ₕ + ½aᵢ₊₁ * Δt  -- kick

    -- N.B. Agda's current compilation strategy for where-bindings
    -- (i.e. λ-lift all the definitions) breaks the sharing of ½aᵢ₊₁ between vᵢ₊₁
    -- and the record, so the 'expensive' calc-½a function will be called twice
