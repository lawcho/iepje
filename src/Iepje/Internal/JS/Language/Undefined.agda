
-- Agda binding to the JavaScript 'undefined' value

module Iepje.Internal.JS.Language.Undefined where

open import Iepje.Internal.JS.Language.PrimitiveTypes as Types using ()

postulate undefined : Types.undefined
{-# COMPILE JS undefined = undefined #-}
