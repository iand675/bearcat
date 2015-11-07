{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Attributes where
import GHC.TypeLits
import GHCJS.Marshal
import GHCJS.Types
import System.IO.Unsafe
import Unsafe.Coerce

type Text = String

data Attribute (v :: AttributeValidity) (e :: Symbol) = Attribute {-# UNPACK #-} !JSString
                                                                  {-# UNPACK #-} !JSVal

attribute :: ToJSVal a => JSString -> a -> Attribute v e
attribute str = Attribute str . unsafePerformIO . toJSVal

data AttributeValidity = Valid | Invalid

type family ValidFor ys x where
  ValidFor '[]       x = 'Invalid
  ValidFor (x ': ys) x = 'Valid
  ValidFor (y ': ys) x = ValidFor ys x

force :: Attribute v e -> Attribute 'Valid e
force = unsafeCoerce

{-
accept_ ["form", "input"]
acceptCharset_ ["form"]
accessKey_
action_ ["form"]
align_ ["applet", "caption", "col", "colgroup", "hr", "iframe", "img", "table", "tbody", "td", "tfoot", "th", "thead", "tr"]
alt_ ["applet", "area", "img", "input"]
async_ ["script"]
autofocus_ ["button", "input", "keygen", "select", "textarea"]
autoplay_ ["audio", "video"]
autosave_ ["input"]
buffered_ ["audio", "video"]
challenge_ ["keygen"]
charset_ ["meta", "script"]
checked_ ["command", "input"]
cite_ ["blockquote", "del", "ins", "q"]
clas_
code_ ["applet"]
codebase_ ["applet"]
cols_ ["textarea"]
colspan_ ["td", "th"]
content_ ["meta"]
contentEditable_
contextMenu_
controls_ ["audio", "video"]
coords_ ["area"]
data_ ["object"
dataAttr_
datetime_ ["del", "ins", "time"]
default_ ["track"]
defer_ ["script"]
dir_
dirname_ ["input", "textarea"]
disabled_ ["button", "command", "fieldset", "input", "keygen", "optgroup", "option", "select", "textarea"]
download_ ["a", "area"]
draggable_
dropzone_
encType_ ["form"]
for_ ["label", "output"]
form_ ["button", "fieldset", "input", "keygen", "label", "meter", "object", "output", "progress", "select", "textarea"]
formAction_ ["input", "button"]
headers_ ["td", "th"]
height_ ["canvas", "embed", "iframe", "img", "input", "object", "video"]
hidden_
high_ ["meter"]
-}

href_ :: Text -> Attribute (ValidFor ["a", "area", "base", "link"] e) e
href_ = attribute "href"

{-
hrefLang_ ["a", "area", "link"]
httpEquiv_ ["meta"]
icon_ ["command"]
id_
ismap_ ["img"]
itemProp_
keyType_ ["keygen"]
kind_ ["track"]
label_ ["track"]
lang_
language_ ["script"]
list_ ["input"]
loop_ ["audio", "bgsound", "marquee", "video"]
low_ ["meter"]
manifest_ ["html"]
max_ ["input", "meter", "progress"]
maxLength_ ["input", "textarea"]
media_ ["a", "area", "link", "source", "style"]
method_ ["form"]
min_ ["input", "meter"]
multiple_ ["input", "select"]
name_ ["button", "form", "fieldset", "iframe", "input", "keygen", "object", "output", "select", "textarea", "map", "meta", "param"]
noValidate_ ["form"]
open_ ["details"]
optimum_ ["meter"]
pattern_ ["input"]
ping_ ["a", "area"]
placeholder_ ["input", "textarea"]
poster_ ["video"]
preload_ ["audio", "video"]
pubdate_ ["time"]
radiogroup_ ["command"]
readOnly_ ["input", "textarea"]
-}

rel_ :: Text -> Attribute (ValidFor ["a", "area", "link"] e) e
rel_ = attribute "rel"

{-
required_ ["input", "select", "textarea"]
reversed_ ["ol"]
rows_ ["textarea"]
rowspan_ ["td", "th"]
sandbox_ ["iframe"]
scope_ ["th"]
scoped_ ["style"]
seamless_ ["iframe"]
selected_ ["option"]
shape_ ["a", "area"]
size_ ["input", "select"]
sizes_ ["link", "img", "source"]
span_ ["col", "colgroup"]
spellcheck_
src_ ["audio", "embed", "iframe", "img", "input", "script", "source", "track", "video"]
srcDoc_ ["iframe"]
srcLang_ ["track"]
srcSet_ ["img"]
start_ ["ol"]
step_ ["input"]
style_
summary_ ["table"]
tabIndex_
target_ ["a", "area", "base", "form"]
title_
type_ ["button", "input", "command", "embed", "object", "script", "source", "style", "menu"]
usemap_ ["img", "input", "object"]
value_ ["button", "option", "input", "li", "meter", "progress", "param"]
width_ ["canvas", "embed", "iframe", "img", "input", "object", "video"]
wrap_ ["textarea"]
-}
