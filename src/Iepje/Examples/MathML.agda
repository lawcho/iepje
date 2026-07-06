
-- A typeset mathematical equation

-- Demonstrates using an obsucre namespace
-- not known to the Iepje library

module Iepje.Examples.MathML where

open import Iepje.Prelude

mathml : IO ⊤
mathml = display "#mathml-app" do
  ns-tag "http://www.w3.org/1998/Math/MathML" "math" do
    tag "mi" do text "F"
    tag "mo" do text "="
    tag "mi" do text "G"
    tag "mfrac" do
      tag "mrow" do
        tag "msub" do
          tag "mi" do text "m"
          tag "mn" do text "1"
        tag "msub" do
          tag "mi" do text "m"
          tag "mn" do text "2"
      tag "msup" do
        tag "mi" do text "r"
        tag "mn" do text "2"
