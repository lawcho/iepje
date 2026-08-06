
-- Agda bindings to the JavaScript File API,
-- https://developer.mozilla.org/en-US/docs/Web/API/File_API


module Iepje.Internal.JS.WebAPIs.File where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.SubTyping using (_extends_)
open import Agda.Builtin.List

postulate
  Blob : Set  -- no documented superclass

  File : Set
  instance sup-File : File extends Blob

  FileList : Set -- no documented superclass

module Blob-methods where

  postulate createObjectURL : Blob → IO string
  {-# COMPILE JS createObjectURL = b => k => k (URL.createObjectURL(b)) #-}

module FileList-methods where

  postulate get-length : FileList → IO number
  {-# COMPILE JS get-length = fl => kn => kn(fl.length) #-}

  postulate item : FileList → number → IO (File ∪ null)
  {-# COMPILE JS item = fl => i => k => k(fl.item(i)) #-}

  postulate to-List : FileList → List File
  {-# COMPILE JS to-List = fl => Array.from(fl) #-}

module File-methods where

  postulate get-name : File → IO string
  {-# COMPILE JS get-name = f => k => k(f.name) #-}

