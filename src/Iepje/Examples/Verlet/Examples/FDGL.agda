
-- Force-directed graph layout

-- inspired by https://d3js.org/d3-force

module Iepje.Examples.Verlet.Examples.FDGL where

open import Iepje.Examples.Verlet.Integrator
open import Iepje.Examples.Verlet.Numeric.All

open import Iepje.Prelude as P hiding (_+_; _*_;_/_;_-_;_<_;_==_; enumerate)

open import Agda.Builtin.Equality

-- Graph-description code
-------------------------

-- TODO: de-hardcode graph
pattern f0 = zero
pattern f1 = suc f0
pattern f2 = suc f1
pattern f3 = suc f2
pattern f4 = suc f3
pattern f5 = suc f4
pattern f5 = suc f4
pattern f6 = suc f5
pattern f7 = suc f6
pattern f8 = suc f7
pattern f9 = suc f8
pattern f10 = suc f9
pattern f11 = suc f10
pattern f12 = suc f10
pattern f13 = suc f12
pattern f14 = suc f13

n = 8

-- Is there an arc from i to j?
arc : (i j : Fin n) → Bool
arc = λ where
  f0 f1 → true
  f0 f2 → true
  f1 f2 → true
  f0 f3 → true
  f3 f4 → true
  f4 f5 → true
  f4 f2 → true
  f5 f6 → true
  f6 f7 → true
  f6 f1 → true
  _ _ → false

-- Is there an arc between i & j, in either direction?
arc-between : (i j : Fin n) → Bool
arc-between i j = arc i j || arc j i

-- Simulation code
------------------

-- Set of forces taken from
-- https://github.com/lawcho/find-cycles/blob/f75d9e0e3f19de77e868adc633129dea1181e8ae/controller.js

arc-free-length = 50
arc-stiffness = 5
pairwise-repulsion-constant = 8000
origin-attraction-constant = 0.1

insulation = 0.99

open VecOps
open Conversion

Index = Fin n

nlinks : Vec Nat n
nlinks = tabulate λ i → sum (tabulate λ j → if arc-between i j then 1 else 0)

module Config where

  Point-Position = Float ²
  Point-Velocity = Float ²
  Point-Acceleration = Float ²
  Point-Force = Float ²

  Position = Vec Point-Position n
  Velocity = Vec Point-Velocity n
  Acceleration = Vec Point-Acceleration n
  Time = Float

  mass = lookup nlinks

  module _ (position : Position) where

    x : Index → Point-Position
    x = lookup position

    origin-force : Index → Point-Force
    origin-force i = - origin-attraction-constant * x i

    -- Nodes are atracted/repelled along arcs, as if arcs were springs
    arc-force : Index → Index → Point-Force
    arc-force i j = if not (arc-between i j) then get-0 else
      let extension = arc-free-length - ∣ x j - x i ∣
      in - arc-stiffness * extension * unit (x j - x i)

    -- All nodes repel each other, as if nodes were charged particles.
    -- Massive nodes repel more strongly, as if they had a higher charge.
    pairwise-force : Index → Index → Point-Force
    pairwise-force i j = if i == j then get-0 else
      - pairwise-repulsion-constant * unit (x j - x i) * mass i * mass j / ∣ x j - x i ∣²

    -- Resultant force on a node
    force : Index → Point-Force
    force i = origin-force i + sum (tabulate λ j → arc-force i j + pairwise-force i j)

    -- Massive nodes accelerate more slowly (F = m a)
    calc-½a : Acceleration
    calc-½a = tabulate λ i → force i / (2 * mass i)

open Leapfrog (record {Config})
open State

-- Reduce the energy in the simulation
cool : State → State
cool r = record r {v = r .v * insulation}

step' : Float → State → State
step' Δt = cool ∘ step Δt

hw = 150 -- half of the scene's width, in pixels

x0 : Vec (Float ²) _
x0 = pure (λ p → hw * (p - (0.5 , 0.5))) <*>
  (
    (0.7054 , 0.3259) ∷
    (0.3030 , 0.5899) ∷
    (0.2030 , 0.7076) ∷
    (0.1245 , 0.7323) ∷
    (0.5443 , 0.7600) ∷
    (0.2940 , 0.3762) ∷
    (0.6456 , 0.8577) ∷
    (0.5383 , 0.3683) ∷
    -- (0.6203 , 0.3979) ∷
    -- (0.1387 , 0.8585) ∷
    []
  )

s0 : State
s0 .v = get-0
s0 .x = x0
s0 .½a = _
s0 .sound = refl

-- GUI code
-----------

fps = 20
max-Δt = 0.1   -- If animation lags more than this, pause the simulation

arc-dots = 15
arc-width = 1
node-radius = 3

disc : Nat → 𝔽 ² → String → Doc ⊤
disc r (cx , cy) color = div do
  style "background" color
  style "border-radius" "50%"
  style "width"  $ show (2 * r) ++ "px"
  style "height" $ show (2 * r) ++ "px"
  style "position" "absolute"
  style "left" $ show (hw + cx - r) ++ "px"
  style "top"  $ show (hw + cy - r) ++ "px"

line : 𝔽 ² → 𝔽 ² → String → Doc ⊤
line start end color = concatDocs $ for (P.enumerate arc-dots) λ i → do
  let θ = i / arc-dots
  disc arc-width ((1 - θ) * start + θ * end) color

view : State → Doc ⊤
view s = do
  style "width" (show (2 * hw) ++ "px")
  style "height" (show (2 * hw) ++ "px")
  style "position" "relative"

  -- Draw arcs
  concatDocs $ for (enumerate n) λ i →
    concatDocs $ for (enumerate n) λ j →
      when (arc i j) do
        line (lookup (s .x) i) (lookup (s .x) j) "rgba(0,0,0,0.3)"

  -- Draw nodes
  concatDocs $ for (enumerate n) λ i → do
    disc 3 (lookup (s .x) i) "blue"

fdgl = play "#verlet-fdgl-app"
  fps
  s0
  view
  (λ _ s → s)
  (λ where Δt → if max-Δt < Δt then (λ x → x) else step' Δt)
