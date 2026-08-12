
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

-- An Agda expression of type (IO A) must compile to a JavaScript expression e such that (for any f):
--
-- 1. Evaluating (e) alone does not perform any side effects
-- 2. Evaluating (e (f)) may perform side effects
-- 3. If (e (f)) returns, it calls f exactly once before returning (with an argument of Agda-type A)
-- 4. (e (f)) does not schedule f to be called at after it returns

-- Some consequences of (3) and (4):
--
--  * IO actions are atomic. An Agda do-block will never be suspended to run other (micro)tasks.
--  * Slow IO actions can freeze a web page UI
--  * No conforming implementation of `await : ∀{A} → Promise A → IO A` exists
--  * Optimizations such as the implementation of _>>=_ below are safe
--  * f is a *synchronous callback*, by Havoc's definition: https://blog.ometer.com/2011/07/24/callbacks-synchronous-and-asynchronous/ 
--  * Conforming bindings no not release Zalgo: https://blog.izs.me/2013/08/designing-apis-for-asynchrony/

-- IO is implemented in CPS because...
--  * CPS prevents execution of side effects at definition-time
--  * Agda's JS backend seems to expect CPS (it calls main with (a => {}))

postulate pure : ∀{A} → A → IO A
{-# COMPILE JS pure = _ => a => ka => ka(a) #-}

postulate _>>=_ : ∀{A B} → IO A → (A → IO B) → IO B

-- This implementation generates a JS call stack with depth O(total number of _>>=_ evalauted)
--  {-# COMPILE JS _>>=_ = _ => _ => ma => a2mb => kb => ma(a => a2mb(a)(b => kb(b))) #-}

-- This implementation generates a JS call stack with depth O(max nesting depth of _>>=_)
{-# COMPILE JS _>>=_ = _ => _ => ma => a2mb => kb =>
  {
    let ar;
    ma(a => ar = a);
    let br;
    a2mb(ar)(b => br = b);
    return kb(br);
  }
#-}
