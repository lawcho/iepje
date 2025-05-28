
-- Incremental rendering algorithm, that only updates parts of
-- the browser DOM which have changed

-- Changes are detected by comparing the old & new Doc versions,
-- both of which have a binary-tree structure.

module Iepje.Internal.Renderer.Merging where

open import Iepje.Internal.Renderer.Replacing

open import Agda.Builtin.Unit

open import Iepje.Internal.Doc.Core ⊤
open import Iepje.Internal.Renderer.vDOM ⊤

open import Iepje.Internal.JS.WebAPIs.DOM as DOM

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences as Ref
open import Iepje.Internal.JS.Language.SubTyping

open import Iepje.Internal.Utils

open import Agda.Builtin.String renaming (primStringEquality to _==_)
open import Agda.Builtin.Maybe

-- Render a Doc into a (vDOM-tracked) HTMLElement,
--  * at the beginning of the HTMLElement
--  * updating the HTMLElement where it differs
--  * stripping styles & attributes
--  * and returning a new vDOM tracker
merge : Parent → vDOM → Doc → IO vDOM
merge parent = λ d d' → do
  cursor ← Ref.new at-beginning
  go cursor d d'
  where module _ (cursor : Ref Cursor) where

    -- Advance the cursor
    curse : {A : Set} → A → {{A extends* DOM.Node}} → IO A
    curse e = do
      Ref.set cursor (after $ up e)
      pure e

    -- Main traversal function
    go : vDOM → Doc → IO vDOM
    go d d'
      using replace ← clear parent d >> insert parent cursor d'
      using when ← if_then_else replace  -- hack, pattern guards wanted instead
      with d
        |  d'

    ... |(vDOM.tag          e    t                       c   )
        |( Doc.tag                    t'                   c')
        = when                  (t == t')
         (vDOM.tag $$ curse e $$ t       $$ merge (up e) c c')

    ... |(vDOM.text          e     t       )
        |( Doc.text                     t' )
        = when                    (t == t')
         (vDOM.text $$ curse e $$  t       )

    ... | (vDOM.append       dL           dR    )
        | ( Doc.append          dL'          dR')
        = (vDOM.append $$ go dL dL' $$ go dR dR')

    ... | _ | _ = replace
    -- -- style is always replaced, to avoid partial deletions
    -- -- onIO is always replaced, since a new function is provided every time

-- Entry point for the renderer.
-- Render a Doc into a (vDOM-tracked) HTMLElement,
--  * at the beginning of the HTMLElement
--  * updating the HTMLElement where it differs
--  * updating all styles and attributes
--  * and returning a new vDOM tracker
render : Parent → vDOM → Doc → IO vDOM
render p vd d' = do
  vd' ← merge p vd d'
  apply-styles-and-attrs p vd'
  pure vd'
   