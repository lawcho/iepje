
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

-- IO is implemented in CPS because...
--  * CPS prevents execution at definition-time
--  * Agda's JS backend seems to expect CPS (it calls main with (a => {}))

postulate pure : ∀{A} → A → IO A
{-# COMPILE JS pure = _ => a => ka => ka(a) #-}

postulate _>>=_ : ∀{A B} → IO A → (A → IO B) → IO B
{-# COMPILE JS _>>=_ = _ => _ => ma => a2mb => kb => ma(a => a2mb(a)(b => kb(b))) #-}
