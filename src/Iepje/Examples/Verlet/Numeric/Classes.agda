
module Iepje.Examples.Verlet.Numeric.Classes where

record Has-+ (A B C : Set) : Set where
  constructor mk-+
  field _+_ : A → B → C
  infixl 25 _+_
open Has-+ {{...}} public

record Has-neg (A B : Set) : Set where
  constructor mk-neg
  field -_ : A → B
  infix 26 -_
open Has-neg {{...}} public

record Has-* (A B C : Set) : Set where
  constructor mk-*
  field _*_ : A → B → C
  infixl 27 _*_
open Has-* {{...}} public

record Has-^ (A B C : Set) : Set where
  constructor mk-^
  field _^_ : A → B → C
  infixr 28 _^_
open Has-^ {{...}} public

record Has-/ (A B C : Set) : Set where
  constructor mk-/
  field _/_ : A → B → C
  infixl 24 _/_
open Has-/ {{...}} public

record Has-< (A B C : Set) : Set where
  constructor mk-<
  field _<_ : A → B → C
  infix 10 _<_
open Has-< {{...}} public

record Has-sub (A B C : Set) : Set where
  constructor mk-s
  field _-_ : A → B → C
  infixl 25 _-_
open Has-sub {{...}} public

record Has-round (A B : Set) : Set where
  constructor mk-r
  field round : A → B
open Has-round {{...}} public

record Has-show (A B : Set) : Set where
  constructor mk-sh
  field show : A → B
open Has-show {{...}} public
