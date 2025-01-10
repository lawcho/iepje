
-- Agda bindings to the JavaScript DOM API,
-- https://developer.mozilla.org/en-US/docs/Web/API/HTML_DOM_API

module Iepje.Internal.JS.WebAPIs.DOM where

open import Iepje.Internal.JS.WebAPIs.CSSOM

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.GlobalObjects
  hiding (String; Number; BigInt; Boolean; Symbol)  -- footguns
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.SubTyping

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

  PointerEvent : Set
  instance sup-PointerEvent : PointerEvent extends MouseEvent

  KeyboardEvent : Set
  instance sup-KeyboardEvent : KeyboardEvent extends UIEvent

  -- Sub-types of Node
  CharacterData : Set
  instance sup-CharacterData : CharacterData extends Node

  Text : Set
  instance sup-Text : Text extends CharacterData

  -- Sub-types of HTMLElement
  -- https://developer.mozilla.org/en-US/docs/Web/API/HTML_DOM_API#html_element_interfaces_2

  HTMLElement : Set
  instance sup-HTMLElement : HTMLElement extends Element

  HTMLBodyElement : Set
  instance sup-HTMLBodyElement : HTMLBodyElement extends HTMLElement

  HTMLButtonElement : Set
  instance sup-HTMLButtonElement : HTMLButtonElement extends HTMLElement

  HTMLUnknownElement : Set
  instance sup-HTMLUnknownElement : HTMLUnknownElement extends HTMLElement

----------------------------------------------------------------
-- Value level bindings with simple types
----------------------------------------------------------------

-- Node

postulate appendChild : Node → Node → IO Node
{-# COMPILE JS appendChild = p => c => kn => kn(p.appendChild(c)) #-}

postulate ownerDocument : Node → IO Document
{-# COMPILE JS ownerDocument = n => kd => kd(n.ownerDocument) #-}

-- Element

postulate set-innerHTML : Element → string → IO string
{-# COMPILE JS set-innerHTML = e => s => ks => ks(e.innerHTML = s) #-}

postulate setAttribute : Element → string → string → IO undefined
{-# COMPILE JS setAttribute = e => sk => sv => kt => kt(e.setAttribute(sk,sv)) #-}

-- This is a var-args function
postulate replaceChildren : Element → List Node → IO undefined
{-# COMPILE JS replaceChildren = el => ns => kt => kt(el.replaceChildren(...(ns))) #-}
-- Works because Agda List s compile to JS arrays

-- HTMLElement

postulate get-style : HTMLElement → IO CSSStyleDeclaration
{-# COMPILE JS get-style = e => ksd => ksd(e.style) #-}

-- Document

postulate document : IO Document  --JS global variable
{-# COMPILE JS document = kd => kd(document) #-}

postulate getElementById : Document → string → IO Element
{-# COMPILE JS getElementById = d => s => ke => ke(d.getElementById(s)) #-}

postulate querySelector : Document → string → IO Element
{-# COMPILE JS querySelector = d => s => ke => ke(d.querySelector(s)) #-}

postulate get-defaultView : Document → IO Window
{-# COMPILE JS get-defaultView = d => kw => kw(d.defaultView) #-}

-- The 'document.body' property can technically contain a HTMLFrameSetElement,
-- but HTMLFrameSetElement is deprecated in HTML5 so this library ignores it
postulate get-body : Document → IO HTMLBodyElement
{-# COMPILE JS get-body = d => kn => kn(d.body) #-}

postulate set-body : Document → HTMLBodyElement → IO HTMLBodyElement
{-# COMPILE JS set-body = d => b => kn => kn(d.body) #-}

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

postulate key : KeyboardEvent → IO string
{-# COMPILE JS key = e => ks => ks(e.key) #-}

----------------------------------------------------------------
-- Value level bindings with dependent types
----------------------------------------------------------------

-- These functions make a best-effort attempt at calculating
-- the most/least precise type produced/consumed by a stringly-typed
-- JavaScript function, and otherwise fall back to a safe
-- (but less useful) super/sub type

-- The most precise subtypes were determined by calling `Object.prototype.tostring`, e.g.
--      Object.prototype.tostring.call(document.createElement("button"))
-- and inspecting the output, e.g.
--      "[object HTMLButtonElement]"

-- The least precise supertypes were determined by reading the MDN docs

-- Calculates the most precise sub-type of Element produced by createElement
Element-of : string → Σ Set (_extends* HTMLElement)
Element-of "button" = HTMLButtonElement , it
Element-of "body"   = HTMLBodyElement   , it
-- Some strings (e.g. "buTtoN") are recognized by the browser,
-- but are not in the list above, so the catch-all case of this function
-- returns HTMLElement rather than HTMLUnknownElement
Element-of _        = HTMLElement       , it

postulate createElement : Document → (s : string) → IO (Element-of s .fst)
{-# COMPILE JS createElement = d => s => k => k(d.createElement(s)) #-}

-- Calculates the most precise sub-type of Event provided to addEventListener's callback
Event-of : string → Σ Set (_extends* Event)
Event-of "click"    = PointerEvent  , it
Event-of "keydown"  = KeyboardEvent , it
Event-of "keyup"    = KeyboardEvent , it
Event-of _          = Event         , it

postulate addEventListener : EventTarget → (s : string) → (Event-of s .fst → IO ⊤) → IO undefined
{-# COMPILE JS addEventListener = et => s => callback => k => k(et.addEventListener(s,e => callback(e)(_=>{}))) #-}
-- The second arg to the callback runs the IO action
 