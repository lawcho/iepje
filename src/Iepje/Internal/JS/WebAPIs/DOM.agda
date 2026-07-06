
-- Agda bindings to the JavaScript DOM API,
-- https://developer.mozilla.org/en-US/docs/Web/API/HTML_DOM_API

module Iepje.Internal.JS.WebAPIs.DOM where

open import Iepje.Internal.JS.WebAPIs.CSSOM

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.GlobalObjects
  hiding (String; Number; BigInt; Boolean; Symbol)  -- footguns
  hiding (null) -- clashes with the name of the type
open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.SubTyping using (_extends_; _extends*'_)

open import Agda.Builtin.Sigma
open import Agda.Builtin.List
open import Agda.Builtin.Unit

----------------------------------------------------------------
-- Type-level bindings
----------------------------------------------------------------

-- The DOM API uses the JavaScript type heirarchies documented at
-- https://developer.mozilla.org/en-US/docs/Web/API/HTML_DOM_API#html_element_interfaces
-- https://developer.mozilla.org/en-US/docs/Web/API/Event

postulate
  EventTarget : Set -- unclear super-class
  Event : Set       -- unclear super-class
  DOMHighResTimeStamp : Set -- unclear super-class

  Node : Set
  instance sup-Node : Node extends EventTarget

  Window : Set
  instance sup-Window : Window extends EventTarget

  Element : Set
  instance sup-Element : Element extends Node

  Document : Set
  instance sup-Document : Document extends Node

  -- Sub-types of Event
  -- https://developer.mozilla.org/en-US/docs/Web/API/Event#interfaces_based_on_event

  UIEvent : Set
  instance sup-UIEvent : UIEvent extends Event

  MouseEvent : Set
  instance sup-MouseEvent : MouseEvent extends UIEvent

  WheelEvent : Set
  instance sup-WheelEvent : WheelEvent extends MouseEvent

  PointerEvent : Set
  instance sup-PointerEvent : PointerEvent extends MouseEvent

  KeyboardEvent : Set
  instance sup-KeyboardEvent : KeyboardEvent extends UIEvent

  -- Sub-types of Node
  CharacterData : Set
  instance sup-CharacterData : CharacterData extends Node

  Text : Set
  instance sup-Text : Text extends CharacterData

  HTMLElement : Set
  instance sup-HTMLElement : HTMLElement extends Element

  HTMLInputElement : Set
  instance sup-HTMLInputElemet : HTMLInputElement extends HTMLElement

  -- SVG

  SVGElement : Set
  instance sup-SVGElement : SVGElement extends Element

  SVGGraphicsElement : Set
  instance sup-SVGGraphicsElement : SVGGraphicsElement extends SVGElement

  SVGSVGElement : Set
  instance sup-SVGSVGElement : SVGSVGElement extends SVGGraphicsElement


----------------------------------------------------------------
-- Value level bindings with simple types
----------------------------------------------------------------

-- Node

postulate appendChild : Node → Node → IO Node
{-# COMPILE JS appendChild = p => c => kn => kn(p.appendChild(c)) #-}

postulate removeChild : Node → Node → IO Node
{-# COMPILE JS removeChild = p => c => kn => kn(p.removeChild(c)) #-}

postulate get-ownerDocument : Node → IO Document
{-# COMPILE JS get-ownerDocument = n => kd => kd(n.ownerDocument) #-}

postulate get-nextSibling : Node → IO (Node ∪ null)
{-# COMPILE JS get-nextSibling = n => knon => knon(n.nextSibling) #-}

postulate get-firstChild : Node → IO (Node ∪ null)
{-# COMPILE JS get-firstChild = n => knon => knon(n.firstChild) #-}

postulate insertBefore : Node → Node → Node ∪ null → IO Node
-- could also return a DocumentFragment, but that is a sub-type of Node
{-# COMPILE JS insertBefore = n1 => n2 => n3 => kn => kn(n1.insertBefore(n2,n3)) #-}

-- Element

postulate setAttribute : Element → string → string → IO undefined
{-# COMPILE JS setAttribute = e => sk => sv => kt => kt(e.setAttribute(sk,sv)) #-}

postulate removeAttribute : Element → string → IO undefined
{-# COMPILE JS removeAttribute = e => sk => kt => kt(e.removeAttribute(sk)) #-}

-- This is a var-args function
postulate replaceChildren : Element → List Node → IO undefined
{-# COMPILE JS replaceChildren = el => ns => kt => kt(el.replaceChildren(...(ns))) #-}
-- Works because Agda List s compile to JS arrays

-- HTMLElement

module HTMLElement-methods where

  postulate get-style : HTMLElement → IO CSSStyleDeclaration
  {-# COMPILE JS get-style = e => ksd => ksd(e.style) #-}

postulate get-value : HTMLInputElement → IO string
{-# COMPILE JS get-value = e => ks => ks(e.value) #-}

-- SVGElement

module SVGElement-methods where

  postulate get-style : SVGElement → IO CSSStyleDeclaration
  {-# COMPILE JS get-style = e => ksd => ksd(e.style) #-}

-- MouseEvent

postulate get-offsetX : MouseEvent → IO number
{-# COMPILE JS get-offsetX = e => kd => kd(e.offsetX) #-}

postulate get-offsetY : MouseEvent → IO number
{-# COMPILE JS get-offsetY = e => kd => kd(e.offsetY) #-}

postulate get-deltaX : WheelEvent → IO number
{-# COMPILE JS get-deltaX = e => kd => kd(e.deltaX) #-}

postulate get-deltaY : WheelEvent → IO number
{-# COMPILE JS get-deltaY = e => kd => kd(e.deltaY) #-}

postulate get-deltaZ : WheelEvent → IO number
{-# COMPILE JS get-deltaZ = e => kd => kd(e.deltaX) #-}

-- Document

postulate document : IO Document  --JS global variable
{-# COMPILE JS document = kd => kd(document) #-}

postulate querySelector : Document → string → IO (Element ∪ null)
{-# COMPILE JS querySelector = d => s => ke => ke(d.querySelector(s)) #-}

postulate get-defaultView : Document → IO Window
{-# COMPILE JS get-defaultView = d => kw => kw(d.defaultView) #-}

postulate createTextNode : Document → string → IO Text
{-# COMPILE JS createTextNode = d => s => kt => kt(d.createTextNode(s)) #-}

-- Window

postulate setInterval : Window → (undefined → IO ⊤) → number → IO number
{-# COMPILE JS setInterval
  = w => callback => millis => kiid => kiid(w.setInterval(_ => callback({})(_=>{}), millis)) #-}
-- The second arg to the callback runs the IO action

postulate requestAnimationFrame : Window → (DOMHighResTimeStamp → IO ⊤) → IO number
{-# COMPILE JS requestAnimationFrame
  = w => callback => kn => kn(w.requestAnimationFrame(t => callback(t)(_=>{}))) #-}
-- The second arg to the callback runs the IO action

-- Event

postulate preventDefault : Event → IO ⊤
{-# COMPILE JS preventDefault = e => kt => kt(e.preventDefault()) #-}

postulate key : KeyboardEvent → IO string
{-# COMPILE JS key = e => ks => ks(e.key) #-}

----------------------------------------------------------------
-- Value level bindings with dependent types
----------------------------------------------------------------

-- The most precise sub-type of Element returned by createElement
postulate Element-of : string → Set

postulate instance
  -- Catch-all case to support un-known Element types
  sup*-Element-of : ∀{s} → Element-of s extends*' HTMLElement
  {-# OVERLAPPABLE sup*-Element-of #-}

  -- Known special cases with more precise sub-types
  -- TODO: move downstream?
  sup*-Element-of-input : Element-of "input" extends*' HTMLInputElement

-- Create a new HTMLElement
postulate createElement : Document → (tag-name : string) → IO (Element-of tag-name)
{-# COMPILE JS createElement = d => s => k => k(d.createElement(s)) #-}

-- The most precise sub-type of Element returned by createElementNS
postulate ElementNS-of : string → string → Set

postulate instance

  -- Catch-all case to support un-known Element types
  sup*-ElementNS-of : ∀{ns s} → ElementNS-of ns s extends*' Element
  {-# OVERLAPPABLE sup*-ElementNS-of #-}

  -- MDN docs: "The createElement() method is simpler if you want to create a plain HTML element."
  sup*-ElementNS-of-html : ∀{s} → ElementNS-of "http://www.w3.org/1999/xhtml" s extends*' Element-of s

  -- Known special cases with more precise sub-types
  -- (determined by calling `createElementNS(ns,s).constructor.name`)
  -- TODO: move downstream?
  sup*-ElementNS-of-svg-svg : ∀{s} → ElementNS-of "http://www.w3.org/2000/svg" s extends*' SVGElement

-- Create a new Element
postulate createElementNS : Document → (namespace : string) (tag-name : string) → IO (ElementNS-of namespace tag-name)
{-# COMPILE JS createElementNS = d => ns => s => k => k(d.createElementNS(ns, s)) #-}

-- The most precise sub-type of Event provided to addEventListener's callback
postulate Event-of : string → Set

postulate instance
  -- Catch-all case to support un-known Event types
  sup*-Event-of : ∀{s} → Event-of s extends*' Event
  {-# OVERLAPPABLE sup*-Event-of #-}

  -- Known special cases with more precise sub-types
  -- TODO: move downstream?
  sup*-Event-of-keydown : Event-of "keydown" extends*' KeyboardEvent
  sup*-Event-of-keyup : Event-of "keyup" extends*' KeyboardEvent
  sup*-Event-of-mousemove : Event-of "mousemove" extends*' MouseEvent
  sup*-Event-of-wheel : Event-of "wheel" extends*' WheelEvent

-- Type of raw JS event listener functions, as described in
-- https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener#the_event_listener_callback
postulate event-listener : string → Set

-- Constructs an event-listener function ready for use in addEventListener and removeEventListener
postulate mk-event-listener : ∀{s} → (Event-of s → IO ⊤) → IO (event-listener s)
{-# COMPILE JS mk-event-listener = _ => f => k => k(e => f(e)(_ => {})) #-}
-- Passing a second argument to f runs the IO action
-- mk-event-listener returns an IO-wrapped value to prevent over-inlining by Agda,
-- which could break JS-function-pointer-equalities

postulate addEventListener : EventTarget → (s : string) → event-listener s → IO undefined
{-# COMPILE JS addEventListener = et => s => callback => k => k(et.addEventListener(s,callback)) #-}
-- The second arg to the callback runs the IO actions

postulate removeEventListener : EventTarget → (s : string) → event-listener s → IO undefined
{-# COMPILE JS removeEventListener = et => s => callback => k => k(et.removeEventListener(s,callback)) #-}
-- The return type of removeEventListener is unclear (MDN docs say 'None'), so 'undefined' is a guess here
