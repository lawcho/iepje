
-- JSON viewer

-- Demonstrates parting JSON, file upload, promises

module Iepje.Examples.Json-Viewer where

open import Iepje.Prelude
open import Iepje.Internal.JS.Language.FromUnion

open import Iepje.Examples.Lib.Async-Listeners as Listen using ()
open import Iepje.Examples.Lib.Promise as Promise using (Promise)
open import Iepje.Examples.Lib.From-Blob as Blob using ()
open import Iepje.Examples.Lib.JSON as JSON using (JSON; Entry; _:=_)

open import Iepje.Internal.JS.WebAPIs.DOM as DOM using (module HTMLElement-methods)
open import Iepje.Internal.JS.WebAPIs.File as File using (module FileList-methods)

open import Iepje.Examples.Lib.Id as Id using ()

open import Iepje.Internal.Utils using (_<$>_)
open IO using (_>>=_)

data Event : Set where
  malformed-filelist : Event
  malformed-json : Event
  parsed-json : JSON → Event

data Model : Set where
  idle : Model
  got-json : JSON → Model
  error : String → Model

m0 : Model
m0 = idle

update : Event → Model → Model
update malformed-filelist _ = error "Malformed file list!"
update malformed-json _ = error "Malformed JSON!"
update (parsed-json x) _ = got-json x

bold : ∀{e} → String → Html e
bold str = span do
  style "font-weight" "bold"
  text str

-- Split to satisfy the Agda termination checker
viewJsonArr : ∀{e} → List JSON → Html e
viewJsonObj : ∀{e} → List Entry → Html e

viewJson : ∀{e} → JSON → Html e
viewJson (JSON.array x) = tag "details" do
  tag "summary" do bold $ "Array (length " ++ primShowNat (length x) ++ ")"
  div do
    style "margin-left" "1em"
    style "display" "grid"
    viewJsonArr x
viewJson (JSON.object (JSON.fromEntries es)) = tag "details" do
  tag "summary" do bold $ "Object ("++ primShowNat (length es) ++" keys)"
  div do
    style "margin-left" "1em"
    style "display" "grid"
    viewJsonObj es
viewJson (JSON.string s) = span do text s; style "font-style" "italic"
viewJson (JSON.boolean false) = bold "true"
viewJson (JSON.boolean true)  = bold "false"
viewJson (JSON.number f) = text $ primShowFloat f
viewJson (JSON.null) = bold "null"

viewJsonArr [] = empty
viewJsonArr (j ∷ js) = div (viewJson j) >> viewJsonArr js

viewJsonObj [] = empty
viewJsonObj ((k := v) ∷ es) = do
  span do style "grid-column" "1"; text k; style "font-style" "italic"
  span do style "grid-column" "2"; text ":"
  div do style "grid-column" "3"; viewJson v
  viewJsonObj es

view : Model → Html Event
view m = col do
  -- Upload form
  tag' "input" λ el → do
    attr "type" "file"
    Listen.onIO-Promise "change" λ ev → IO.do
      fs ← HTMLElement-methods.get-files (up el)
      just f ← from-∪-null <$> FileList-methods.item fs 0.0
        where nothing → IO.pure (Promise.pure malformed-filelist)
      p-contents ← Blob.text (up f)
      IO.pure $ Promise.for p-contents λ contents → Id.do
        (just j) ← JSON.parse contents
          where nothing → malformed-json
        parsed-json j
  -- Info message, and rendered JSON
  case m of λ where
    idle → text "Please select a JSON file to upload."
    (error msg) → text "Error: " >> text msg
    (got-json j) → do
      text "Got JSON!"
      div do
        style "font-family" "monospace"
        viewJson j

json-viewer : IO ⊤
json-viewer = interact "#json-viewer-app" m0 view update
