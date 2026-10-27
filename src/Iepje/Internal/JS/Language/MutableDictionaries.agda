
-- Mutable string-keyed maps

-- * Backed by null-prototype JS objects
-- * Accessible only in IO

module Iepje.Internal.JS.Language.MutableDictionaries where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils

open import Agda.Builtin.Bool
open import Agda.Builtin.String

-- Type of mutable dictionary
postulate Dict : Set → Set

-- Create a new mutable dictionary
postulate empty : ∀{A} → IO (Dict A)
{-# COMPILE JS empty = _ => _ => Object.create(null) #-}

-- Get the value at a key (with default in case of failure)
postulate get : ∀{A B : Set} → String → (A → B) → B → Dict A → IO B
{-# COMPILE JS get = _ => _ => k => f => d => dict => _ => k in dict ? f(dict[k]) : d #-}

-- Add or replace the value at a key
postulate set : ∀{A : Set} → String → A → Dict A → IO A
{-# COMPILE JS set = _ => k => v => dict => _ => dict[k] = v #-}

-- Delete the value at a key
postulate unset : ∀{A : Set} → String → Dict A → IO Bool -- Always true
{-# COMPILE JS unset = _ => k => dict => _ => delete dict[k] #-}
