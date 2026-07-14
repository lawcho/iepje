
-- Bindings to the Blob decoders which rely on Promise s

module Iepje.Examples.Lib.From-Blob where

open import Agda.Builtin.String

open import Iepje.Internal.JS.WebAPIs.File using (Blob)
open import Iepje.Internal.JS.Language.IO using (IO)

open import Iepje.Examples.Lib.Promise using (Promise)

postulate text : Blob → IO (Promise String)
{-# COMPILE JS text = b => k => k(b.text()) #-}
