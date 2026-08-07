
-- Interface to Browser-provided JSON parser

module Iepje.Examples.Lib.JSON where

open import Agda.Builtin.List
open import Agda.Builtin.Sigma
open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Agda.Builtin.Float
open import Agda.Builtin.Maybe

open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.PrimitiveTypes using (undefined)
open import Iepje.Internal.JS.Language.FromUnion using (from-∪-undefined)

-- String-keyed dictionaries
record Object : Set
-- backed by a JS object (with null prototype unless set in JSON)

-- JSON objects
data JSON : Set where
  array : List JSON → JSON
  object : Object → JSON
  string : String → JSON
  boolean : Bool → JSON
  number : Float → JSON
  null : JSON
-- backed by corresponding JavaScript value
{-# COMPILE JS array = x => x #-}
{-# COMPILE JS object = x => x #-}
{-# COMPILE JS string = x => x #-}
{-# COMPILE JS boolean = x => x #-}
{-# COMPILE JS number = x => x #-}
{-# COMPILE JS null = null #-}
{-# COMPILE JS JSON = function(x,v)
  {
    if (x === null)
    {
      return v["null"]();
    }
    if (x instanceof Array)
    {
      return v["array"](x);
    }
    else if (typeof x === 'object')
    {
      return v["object"](x);
    }
    else if (typeof x === 'string')
    {
      return v["string"](x);
    }
    else if (typeof x === 'boolean')
    {
      return v["boolean"](x);
    }
    else if (typeof x === 'number')
    {
      return v["number"](x);
    }
    else {
      throw new Error ("Bug in Agda FFI binding for JSON!")
    }
  }
#-}

-- JSON key-value pairs
record Entry : Set where
  inductive
  constructor _:=_
  field key : String
  field value : JSON

{-# COMPILE JS Entry = ( ([k,v],visitor) => visitor["_:=_"](k,v) ) #-}
{-# COMPILE JS _:=_ = k => v => [k,v] #-}
{-# COMPILE JS Entry.key = ([k,v]) => k #-}
{-# COMPILE JS Entry.value = ([k,v]) => v #-}
-- Credits:
-- https://github.com/stickyPiston/avea/blob/6f67a3265ae99d5136aac7dc8c5fd6797ab1a499/docs/JsFFI.lagda.md

-- JSON Objects
record Object where
  inductive
  constructor fromEntries
  field entries : List Entry

{-# COMPILE JS Object = ( (jo, visitor) => visitor["fromEntries"](Object.entries(jo)) ) #-}
{-# COMPILE JS fromEntries = arr => Object.fromEntries(arr) #-}
{-# COMPILE JS Object.entries = jo => Object.entries(jo) #-}

-- Direct object lookup, without translation to array
postulate lookup-raw : String → Object → (JSON ∪ undefined)
{-# COMPILE JS lookup-raw = k => jo => jo[k] #-}

lookup : String → Object → Maybe JSON
lookup s jo = from-∪-undefined (lookup-raw s jo)

-- JSON-parsing function
postulate parse : String → Maybe JSON
{-# COMPILE JS parse = s =>
  {
    try
      {
        return z_jAgda_Agda_Builtin_Maybe["Maybe"]["just"]
          (JSON.parse(s));
      }
    catch (error)
      {
        return z_jAgda_Agda_Builtin_Maybe["Maybe"]["nothing"];
      }
  }
#-}

-- JSON-printing function
postulate stringify : JSON → String
{-# COMPILE JS stringify = JSON.stringify #-}
