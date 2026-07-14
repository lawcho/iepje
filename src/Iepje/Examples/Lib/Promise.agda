
-- ES6 promises

module Iepje.Examples.Lib.Promise where

open import Iepje.Prelude using (IO)

postulate Promise : Set → Set

postulate pure : ∀{A} → A → Promise A
{-# COMPILE JS pure = _ => a => new Promise((resolve, reject) => resolve(a)) #-}

postulate map : ∀{A B} → (A → B) → Promise A → Promise B
{-# COMPILE JS map = _ => _ => f => pa => pa.then(f) #-}

for : ∀{A B} → Promise A → (A → B) → Promise B
for pa f = map f pa

postulate join : ∀{A} → Promise (Promise A) → Promise A
{-# COMPILE JS join = _ => ppa => ppa.try(pa => pa) #-}

postulate sequence-IO : ∀{A} → Promise (IO A) → IO (Promise A)
{-# COMPILE JS sequence-IO = _ => pkka => kpa =>
  pkka.then
    (kka => kka
      (a => kpa
        (new Promise
          ( (resolve, reject) => resolve(a)
          )
        )
      )
    )
#-}

mapIO : ∀{A B} → (A → IO B) → Promise A → IO (Promise B)
mapIO f pa = sequence-IO (map f pa)

forIO : ∀{A B} → Promise A → (A → IO B) → IO (Promise B)
forIO pa f = mapIO f pa
