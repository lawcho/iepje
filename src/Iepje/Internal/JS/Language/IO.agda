
-- Implementation of the IO monad, for use with Agda's JS backend

module Iepje.Internal.JS.Language.IO where

-- This module deliberately does *not*
--
-- import Agda.Builtin.IO
--
-- Instead, IO is postualted, since...
-- * Agda.Builtin.IO.IO might be used by other libraries
-- * Which might use another JS-side implementation
-- * defining `IO = Agda.Builtin.IO.IO` would give users a false sense of compatability
-- * If compatability is desired, users can postulate conversion functions themselves
postulate IO : Set → Set

-- IO is implemented in delay-passing style, i.e.
--  (IO A) ≈ Unit -[effectful]-> A

-- IO is implemented in delay-passing style since...
--  * Delay-passing prevents execution of side effects at definition-time
--  * Agda's JS backend is compatible with delay passing IO (it calls main with (a => {}))
--  * Delay-passing has a simpler contract (& simpler COMPILE JS pragmas) that continuation-passing

postulate pure : ∀{A} → A → IO A
{-# COMPILE JS pure = _ => a => _ => (a) #-}

postulate _>>=_ : ∀{A B} → IO A → (A → IO B) → IO B

{-# COMPILE JS _>>=_ = _ => _ => ma => a2mb => _ => a2mb(ma())() #-}
