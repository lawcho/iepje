
-- Glue code to run all the examples

-- Compile with
-- agda src/Iepje/Examples/Gallery.agda --js --js-es6 --compile-dir _build/js/

module Iepje.Examples.Gallery where

open import Iepje.Examples.Hello
open import Iepje.Examples.Counter
open import Iepje.Examples.Tic-Tac-Toe
open import Iepje.Examples.Stopwatch
open import Iepje.Examples.Pong
open import Iepje.Examples.Dragger
open import Iepje.Examples.Quiz
open import Iepje.Examples.Conway
open import Iepje.Examples.One-Body
open import Iepje.Examples.FDGL
open import Iepje.Examples.Metaballs
open import Iepje.Examples.MathML
open import Iepje.Examples.Scatterplot

open import Iepje.Prelude using (IO;⊤)
open import Iepje.Internal.Utils using (_>>_)

main : IO ⊤
main = do
  hello 
  counter
  tic-tac-toe
  stopwatch
  pong
  dragger
  quiz
  conway
  one-body
  fdgl
  metaballs
  mathml
  scatterplot
