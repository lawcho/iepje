
-- Extra float functions not provided by Agda.Builtin.Float

module Iepje.Examples.Lib.Float where

open import Agda.Builtin.Float
open import Agda.Builtin.Nat
open import Agda.Builtin.String

postulate max min : Float → Float → Float
{-# COMPILE JS max = f1 => f2 => Math.max(f1, f2) #-}
{-# COMPILE JS min = f1 => f2 => Math.min(f1, f2) #-}

postulate +Infinity -Infinity : Float
{-# COMPILE JS +Infinity = 1.0/0.0 #-}
{-# COMPILE JS -Infinity = -1.0/0.0 #-}

postulate toFixed : Float → Nat → String
{-# COMPILE JS toFixed = f => n => f.toFixed(Number(n)) #-}

postulate toExponential : Float → Nat → String
{-# COMPILE JS toExponential = f => n => f.toExponential(Number(n)) #-}

postulate log10 : Float → Float
{-# COMPILE JS log10 = f => Math.log10(f) #-}

postulate floor ceil trunc : Float → Float
{-# COMPILE JS floor = Math.floor #-}
{-# COMPILE JS ceil = Math.ceil #-}
{-# COMPILE JS trunc = Math.trunc #-}
