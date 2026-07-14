
-- Agda bindings to the JavaScript File API,
-- https://developer.mozilla.org/en-US/docs/Web/API/File_API


module Iepje.Internal.JS.WebAPIs.File where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.SubTyping using (_extends_)

postulate
  Blob : Set  -- no documented superclass

  File : Set
  instance sup-File : File extends Blob

  FileList : Set -- no documented superclass

module FileList-methods where

  postulate get-length : FileList → IO number
  {-# COMPILE JS get-length = fl => kn => kn(fl.length) #-}

  postulate item : FileList → number → IO (File ∪ null)
  {-# COMPILE JS item = fl => i => k => k(fl.item(i)) #-}

module File-methods where

  postulate get-name : File → IO string
  {-# COMPILE JS get-name = f => k => k(f.name) #-}

