
module Iepje.Examples.Lib.Numeric.Classes where

record Has-+ (A B C : Set) : Set where
  constructor mk-+
  field _+_ : A → B → C
  infixl 25 _+_
open Has-+ {{...}} public

record Has-neg (A B : Set) : Set where
  constructor mk-neg
  field neg : A → B
  -_ = neg
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

record Has-pure (A B : Set) : Set where
   constructor mk-pure
   field pure : A → B
open Has-pure {{...}} public

record Has-<*> (A B C : Set) : Set where
   constructor mk-<*>
   field _<*>_ : A → B → C
   infixl 20 _<*>_
open Has-<*> {{...}} public

record Has-∙ (A B C : Set) : Set where
  constructor mk-∙
  field _∙_ : A → B → C
open Has-∙ {{...}} public

record Has-0 (A : Set) : Set where
  constructor mk-0
  field get-0 : A
open Has-0 {{...}} public

record Has-1 (A : Set) : Set where
  constructor mk-1
  field get-1 : A
open Has-1 {{...}} public

record Has-== (A B C : Set) : Set where
  constructor mk-==
  field _==_ : A → B → C
open Has-== {{...}} public
