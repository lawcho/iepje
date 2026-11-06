
-- User-friendly combinators for writing Html s

module Iepje.Internal.Doc.Combinators where

open import Iepje.Internal.Utils as Utils using ()

open import Iepje.Internal.Doc.Core
open import Iepje.Internal.JS.WebAPIs.DOM as DOM using ()
open import Iepje.Internal.JS.Language.IO as IO using (IO; pure; _>>=_)
open import Iepje.Internal.JS.Language.SubTyping using (up)
open import Iepje.Internal.Renderer.vDOM as vDOM using (vDOM)
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.Renderer.Delete

open import Iepje.Internal.Doc.Has-style
open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.Sigma
open import Agda.Builtin.Bool
open import Agda.Builtin.Maybe
open import Agda.Builtin.Unit
open import Agda.Builtin.Equality

private variable ns : String
private variable e a b : Set

module _ where opaque

  unfolding Doc

  open Utils
  open Cursor {{...}}
  open EventConsumer {{...}}

  private
    ε : vDOM ns
    ε = vDOM.empty

  ns-tag' : (ns' tag-name : String) → (DOM.ElementNS-of ns' tag-name → Doc e ns') → Doc e ns
  ns-tag' n₁ t₁ _ (vDOM.tag n₀ t₀ _ _) with n₀ ≟ₛ n₁ | t₀ ≟ₛ t₁
  ns-tag' _  _  f (vDOM.tag n₀ t₀ el v)  | just refl | just refl = do
    vDOM.tag n₀ t₀ <$> curse el <*> (f el v =<<ᵢ init el)
  ns-tag' n t f v | _ | _ = delete v >> do  -- duplicate case; want guards
    el ← DOM.createElementNS doc n t
    vDOM.tag n t <$> insert el <*> (f el ε =<<ᵢ init el)
  ns-tag' n t f v = delete v >> do
    el ← DOM.createElementNS doc n t
    vDOM.tag n t <$> insert el <*> (f el ε =<<ᵢ init el)

  text : String → Doc e ns
  text msg₁ (vDOM.text msg₀ el) with msg₀ ==ₛ msg₁
  ... | true  = vDOM.text msg₀ <$> curse el
  text msg v | _ = do delete v; vDOM.text msg <$> (insert =<< DOM.createTextNode doc msg) -- DC;WG
  text msg v     = do delete v; vDOM.text msg <$> (insert =<< DOM.createTextNode doc msg)

  attr : String → String → Doc e ns -- applies to the *parent* element
  attr k v vd = do delete vd; vDOM.attr k <$> curse-attr k v

  style : {{Has-style ns}} → String → String → Doc e ns -- applies to the *parent* element
  style k v vd = do delete vd; vDOM.attr k <$> curse-style k v

  with-parent : (DOM.Element → Doc e ns) → Doc e ns
  with-parent k = k (up parent)

  with-document : (DOM.Document → Doc e ns) → Doc e ns
  with-document k = k doc

  with-submit-event : ((e → IO ⊤) → Doc e ns) → Doc e ns
  with-submit-event k = k submit-event

  on''' : (target : DOM.EventTarget) (js-event-name : String)
    → (DOM.Event-of js-event-name → IO ⊤)
    → Doc e ns
  on''' t n k v = delete v >> do
    l ← DOM.mk-event-listener k
    DOM.addEventListener t n l
    pure $ vDOM.on''' t n l
  -- Cannot reliably test if new k = old k (since new k unfrozen), so always delete & replace

  append : Doc e ns → Doc e ns → Doc e ns
  append d1 d2 (vDOM.append v1 v2) = vDOM.append <$> d1 v1 <*> d2 v2
  append d1 d2 v      = do delete v; vDOM.append <$> d1 ε  <*> d2 ε

  empty : Doc e ns
  empty v = delete v >> pure vDOM.empty

  array : List (Doc e ns) → Doc e ns
  array ds v@(vDOM.array vs) with length ds ≟ₙ length vs
  ... | just p = do           vDOM.array <$> zipWithA (λ d v → d v) ds vs p
  ... | _      = do delete v; vDOM.array <$> mapA     (λ d   → d ε) ds -- DC;WG
  array ds v   = do delete v; vDOM.array <$> mapA     (λ d   → d ε) ds

open Utils hiding (_>>_)

_>>_ = append
infixl 20 _>>_

-- Attach a
--    synchronous effectful filtering
--  event listener to
--    an arbitrary element
onIO'-Maybe : ∀{ns e}
  → (target : DOM.EventTarget) (js-event-name : String)
  → (DOM.Event-of js-event-name → IO (Maybe e))
  → Doc e ns
onIO'-Maybe tgt s l = with-submit-event λ submit-event →
  on''' tgt s λ e →
  l e IO.>>= λ where
    (just v) → submit-event v
    nothing → pure tt

-- Attach synchronous effectful event listener to arbitrary element
onIO' : ∀{ns e}
  → (target : DOM.EventTarget) (js-event-name : String)
  → (DOM.Event-of js-event-name → IO e)
  → Doc e ns
onIO' tgt s l = onIO'-Maybe tgt s λ e → just <$> l e

-- Attach synchronous pure listener to arbitrary element
on' : ∀{ns} → (target : DOM.EventTarget) (js-event-name : String)
    → (DOM.Event-of js-event-name → e)
    → Doc e ns
on' tgt s l = onIO' tgt s (pure ∘ l)

-- Attach synchronous effectful listener to parent element (or root doc.)
onIO doc-onIO : ∀{ns} → (js-event-name : String)
    → (DOM.Event-of js-event-name → IO e)
    → Doc e ns
onIO s l = with-parent λ p → onIO' (up p) s l
doc-onIO s l = with-document λ d → onIO' (up d) s l


-- Attach synchronous effectful filtering listener to parent element (or root doc.)
onIO-Maybe doc-onIO-Maybe : ∀{ns} → (js-event-name : String)
    → (DOM.Event-of js-event-name → IO (Maybe e))
    → Doc e ns
onIO-Maybe s l = with-parent λ p → onIO'-Maybe (up p) s l
doc-onIO-Maybe s l = with-document λ d → onIO'-Maybe (up d) s l

-- Attach synchronous pure listener to parent element (or root doc.)
on doc-on : ∀{ns} → (js-event-name : String)
    → (DOM.Event-of js-event-name → e)
    → Doc e ns
on s h = onIO s (pure ∘ h)
doc-on s h = doc-onIO s (pure ∘ h)

on-key-down on-key-up : ∀{ns} → (String → Maybe e) → Doc e ns
on-key-down decode = onIO-Maybe "keydown" λ e → decode <$> DOM.key (up e)
on-key-up   decode = onIO-Maybe "keyup"   λ e → decode <$> DOM.key (up e)

-- Same namespace, passes element
tag' : ∀{ns} t → (DOM.ElementNS-of ns t → Doc e ns) → Doc e ns
tag' t d = ns-tag' _ t d

-- Changes namespace, no access to element
ns-tag : ∀{ns} ns' → String → Doc  e ns' → Doc e ns
ns-tag ns t d = ns-tag' ns t λ _ → d

-- Same namespace, no access to element
tag : ∀{ns} → String → Doc e ns → Doc e ns
tag t d = ns-tag _ t d

div : Html e → Html e
div = tag "div"

span : Html e → Html e
span = tag "span"

button : e → Html e → Html e
button e inner = tag "button" do
  on "click" λ _ → e
  inner

table : Html e → Html e
table = tag "table"

tr : Html e → Html e
tr = tag "tr"

td : Html e → Html e
td = tag "td"

row : Html e → Html e
row inner = div do
  style "display" "flex"
  style "flex-direction" "row"
  inner

col : Html e → Html e
col inner = div do
  style "display" "flex"
  style "flex-direction" "column"
  inner

br : Html e
br = tag "br" empty

svg : Svg e → Html e
svg s = ns-tag _ "svg" s

opaque
  unfolding Doc

  -- Change the event type of a Doc
  mapDocIO : ∀{ns A B} → (A → IO B) → Doc A ns → Doc B ns
  mapDocIO f d {{event-consumer = mk-EC kb}} =
             d {{event-consumer = mk-EC (kb <=< f)}}

forDocIO : ∀{a b} → Html a → (a → IO b) → Html b
forDocIO d f = mapDocIO f d

mapHtml : ∀{a b} → (a → b) → Html a → Html b
mapHtml f d = mapDocIO (pure ∘ f) d

forHtml : ∀{a b} → Html a → (a → b) → Html b
forHtml d f = forDocIO d (pure ∘ f)

when : Bool → Doc a ns → Doc a ns
when true a = a
when false _ = empty 

concatDocs : List (Doc a ns) → Doc a ns -- Legacy
concatDocs = array
