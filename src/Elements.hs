{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Elements where
import Attributes (Attribute)
import Control.Monad.Base
import Data.Proxy
import GHCJS.DOM.Types (Element)
import Incremental

a_ :: MonadBase Incremental m => m (ElementOptions "a")
  -> [Attribute "a"]
  -> m a
  -> m (Element, a)
a_ = element (Proxy :: Proxy "a") "a"

base_ :: MonadBase Incremental m => m (ElementOptions "base")
  -> [Attribute "base"]
  -> m Element
base_ = elementVoid (Proxy :: Proxy "base") "base"

head_ :: MonadBase Incremental m => m (ElementOptions "head")
  -> [Attribute "head"]
  -> m a
  -> m (Element, a)
head_ = element (Proxy :: Proxy "head") "head"

style_ :: MonadBase Incremental m => m (ElementOptions "style")
  -> [Attribute "style"]
  -> m a
  -> m (Element, a)
style_ = element (Proxy :: Proxy "style") "style"

title_ :: MonadBase Incremental m => m (ElementOptions "title")
  -> [Attribute "title"]
  -> m a
  -> m (Element, a)
title_ = element (Proxy :: Proxy "title") "title"

address_ :: MonadBase Incremental m => m (ElementOptions "address")
  -> [Attribute "address"]
  -> m a
  -> m (Element, a)
address_ = element (Proxy :: Proxy "address") "address"

article_ :: MonadBase Incremental m => m (ElementOptions "article")
  -> [Attribute "article"]
  -> m a
  -> m (Element, a)
article_ = element (Proxy :: Proxy "article") "article"

footer_ :: MonadBase Incremental m => m (ElementOptions "footer")
  -> [Attribute "footer"]
  -> m a
  -> m (Element, a)
footer_ = element (Proxy :: Proxy "footer") "footer"

h1_ :: MonadBase Incremental m => m (ElementOptions "h1")
  -> [Attribute "h1"]
  -> m a
  -> m (Element, a)
h1_ = element (Proxy :: Proxy "h1") "h1"

h2_ :: MonadBase Incremental m => m (ElementOptions "h2")
  -> [Attribute "h2"]
  -> m a
  -> m (Element, a)
h2_ = element (Proxy :: Proxy "h2") "h2"

h3_ :: MonadBase Incremental m => m (ElementOptions "h3")
  -> [Attribute "h3"]
  -> m a
  -> m (Element, a)
h3_ = element (Proxy :: Proxy "h3") "h3"

h4_ :: MonadBase Incremental m => m (ElementOptions "h4")
  -> [Attribute "h4"]
  -> m a
  -> m (Element, a)
h4_ = element (Proxy :: Proxy "h4") "h4"

h5_ :: MonadBase Incremental m => m (ElementOptions "h5")
  -> [Attribute "h5"]
  -> m a
  -> m (Element, a)
h5_ = element (Proxy :: Proxy "h5") "h5"

h6_ :: MonadBase Incremental m => m (ElementOptions "h6")
  -> [Attribute "h6"]
  -> m a
  -> m (Element, a)
h6_ = element (Proxy :: Proxy "h6") "h6"

hgroup_ :: MonadBase Incremental m => m (ElementOptions "hgroup")
  -> [Attribute "hgroup"]
  -> m a
  -> m (Element, a)
hgroup_ = element (Proxy :: Proxy "hgroup") "hgroup"

nav_ :: MonadBase Incremental m => m (ElementOptions "nav")
  -> [Attribute "nav"]
  -> m a
  -> m (Element, a)
nav_ = element (Proxy :: Proxy "nav") "nav"

section_ :: MonadBase Incremental m => m (ElementOptions "section")
  -> [Attribute "section"]
  -> m a
  -> m (Element, a)
section_ = element (Proxy :: Proxy "section") "section"

dd_ :: MonadBase Incremental m => m (ElementOptions "dd")
  -> [Attribute "dd"]
  -> m a
  -> m (Element, a)
dd_ = element (Proxy :: Proxy "dd") "dd"

div_ :: MonadBase Incremental m => m (ElementOptions "div")
  -> [Attribute "div"]
  -> m a
  -> m (Element, a)
div_ = element (Proxy :: Proxy "div") "div"

dl_ :: MonadBase Incremental m => m (ElementOptions "dl")
  -> [Attribute "dl"]
  -> m a
  -> m (Element, a)
dl_ = element (Proxy :: Proxy "dl") "dl"

dt_ :: MonadBase Incremental m => m (ElementOptions "dt")
  -> [Attribute "dt"]
  -> m a
  -> m (Element, a)
dt_ = element (Proxy :: Proxy "dt") "dt"

figcaption_ :: MonadBase Incremental m => m (ElementOptions "figcaption")
  -> [Attribute "figcaption"]
  -> m a
  -> m (Element, a)
figcaption_ = element (Proxy :: Proxy "figcaption") "figcaption"

figure_ :: MonadBase Incremental m => m (ElementOptions "figure")
  -> [Attribute "figure"]
  -> m a
  -> m (Element, a)
figure_ = element (Proxy :: Proxy "figure") "figure"

hr_ :: MonadBase Incremental m => m (ElementOptions "hr")
  -> [Attribute "hr"]
  -> m Element
hr_ = elementVoid (Proxy :: Proxy "hr") "hr"

li_ :: MonadBase Incremental m => m (ElementOptions "li")
  -> [Attribute "li"]
  -> m a
  -> m (Element, a)
li_ = element (Proxy :: Proxy "li") "li"

img_ :: MonadBase Incremental m => m (ElementOptions "img")
  -> [Attribute "img"]
  -> m Element
img_ = elementVoid (Proxy :: Proxy "img") "img"

main_ :: MonadBase Incremental m => m (ElementOptions "main")
  -> [Attribute "main"]
  -> m a
  -> m (Element, a)
main_ = element (Proxy :: Proxy "main") "main"

ol_ :: MonadBase Incremental m => m (ElementOptions "ol")
  -> [Attribute "ol"]
  -> m a
  -> m (Element, a)
ol_ = element (Proxy :: Proxy "ol") "ol"

p_ :: MonadBase Incremental m => m (ElementOptions "p")
  -> [Attribute "p"]
  -> m a
  -> m (Element, a)
p_ = element (Proxy :: Proxy "p") "p"

pre_ :: MonadBase Incremental m => m (ElementOptions "pre")
  -> [Attribute "pre"]
  -> m a
  -> m (Element, a)
pre_ = element (Proxy :: Proxy "pre") "pre"

ul_ :: MonadBase Incremental m => m (ElementOptions "ul")
  -> [Attribute "ul"]
  -> m a
  -> m (Element, a)
ul_ = element (Proxy :: Proxy "ul") "ul"

abbr_ :: MonadBase Incremental m => m (ElementOptions "abbr")
  -> [Attribute "abbr"]
  -> m a
  -> m (Element, a)
abbr_ = element (Proxy :: Proxy "abbr") "abbr"

b_ :: MonadBase Incremental m => m (ElementOptions "b")
  -> [Attribute "b"]
  -> m a
  -> m (Element, a)
b_ = element (Proxy :: Proxy "b") "b"

bdi_ :: MonadBase Incremental m => m (ElementOptions "bdi")
  -> [Attribute "bdi"]
  -> m a
  -> m (Element, a)
bdi_ = element (Proxy :: Proxy "bdi") "bdi"

bdo_ :: MonadBase Incremental m => m (ElementOptions "bdo")
  -> [Attribute "bdo"]
  -> m a
  -> m (Element, a)
bdo_ = element (Proxy :: Proxy "bdo") "bdo"

br_ :: MonadBase Incremental m => m (ElementOptions "br")
  -> [Attribute "br"]
  -> m Element
br_ = elementVoid (Proxy :: Proxy "br") "br"

cite_ :: MonadBase Incremental m => m (ElementOptions "cite")
  -> [Attribute "cite"]
  -> m a
  -> m (Element, a)
cite_ = element (Proxy :: Proxy "cite") "cite"

code_ :: MonadBase Incremental m => m (ElementOptions "code")
  -> [Attribute "code"]
  -> m a
  -> m (Element, a)
code_ = element (Proxy :: Proxy "code") "code"

data_ :: MonadBase Incremental m => m (ElementOptions "data")
  -> [Attribute "data"]
  -> m a
  -> m (Element, a)
data_ = element (Proxy :: Proxy "data") "data"

dfn_ :: MonadBase Incremental m => m (ElementOptions "dfn")
  -> [Attribute "dfn"]
  -> m a
  -> m (Element, a)
dfn_ = element (Proxy :: Proxy "dfn") "dfn"

em_ :: MonadBase Incremental m => m (ElementOptions "em")
  -> [Attribute "em"]
  -> m a
  -> m (Element, a)
em_ = element (Proxy :: Proxy "em") "em"

i_ :: MonadBase Incremental m => m (ElementOptions "i")
  -> [Attribute "i"]
  -> m a
  -> m (Element, a)
i_ = element (Proxy :: Proxy "i") "i"

kbd_ :: MonadBase Incremental m => m (ElementOptions "kbd")
  -> [Attribute "kbd"]
  -> m a
  -> m (Element, a)
kbd_ = element (Proxy :: Proxy "kbd") "kbd"

mark_ :: MonadBase Incremental m => m (ElementOptions "mark")
  -> [Attribute "mark"]
  -> m a
  -> m (Element, a)
mark_ = element (Proxy :: Proxy "mark") "mark"

q_ :: MonadBase Incremental m => m (ElementOptions "q")
  -> [Attribute "q"]
  -> m a
  -> m (Element, a)
q_ = element (Proxy :: Proxy "q") "q"

rp_ :: MonadBase Incremental m => m (ElementOptions "rp")
  -> [Attribute "rp"]
  -> m a
  -> m (Element, a)
rp_ = element (Proxy :: Proxy "rp") "rp"

rt_ :: MonadBase Incremental m => m (ElementOptions "rt")
  -> [Attribute "rt"]
  -> m a
  -> m (Element, a)
rt_ = element (Proxy :: Proxy "rt") "rt"

rtc_ :: MonadBase Incremental m => m (ElementOptions "rtc")
  -> [Attribute "rtc"]
  -> m a
  -> m (Element, a)
rtc_ = element (Proxy :: Proxy "rtc") "rtc"

ruby_ :: MonadBase Incremental m => m (ElementOptions "ruby")
  -> [Attribute "ruby"]
  -> m a
  -> m (Element, a)
ruby_ = element (Proxy :: Proxy "ruby") "ruby"

s_ :: MonadBase Incremental m => m (ElementOptions "s")
  -> [Attribute "s"]
  -> m a
  -> m (Element, a)
s_ = element (Proxy :: Proxy "s") "s"

samp_ :: MonadBase Incremental m => m (ElementOptions "samp")
  -> [Attribute "samp"]
  -> m a
  -> m (Element, a)
samp_ = element (Proxy :: Proxy "samp") "samp"

small_ :: MonadBase Incremental m => m (ElementOptions "small")
  -> [Attribute "small"]
  -> m a
  -> m (Element, a)
small_ = element (Proxy :: Proxy "small") "small"

span_ :: MonadBase Incremental m => m (ElementOptions "span")
  -> [Attribute "span"]
  -> m a
  -> m (Element, a)
span_ = element (Proxy :: Proxy "span") "span"

strong_ :: MonadBase Incremental m => m (ElementOptions "strong")
  -> [Attribute "strong"]
  -> m a
  -> m (Element, a)
strong_ = element (Proxy :: Proxy "strong") "strong"

sub_ :: MonadBase Incremental m => m (ElementOptions "sub")
  -> [Attribute "sub"]
  -> m a
  -> m (Element, a)
sub_ = element (Proxy :: Proxy "sub") "sub"

sup_ :: MonadBase Incremental m => m (ElementOptions "sup")
  -> [Attribute "sup"]
  -> m a
  -> m (Element, a)
sup_ = element (Proxy :: Proxy "sup") "sup"

time_ :: MonadBase Incremental m => m (ElementOptions "time")
  -> [Attribute "time"]
  -> m a
  -> m (Element, a)
time_ = element (Proxy :: Proxy "time") "time"

u_ :: MonadBase Incremental m => m (ElementOptions "u")
  -> [Attribute "u"]
  -> m a
  -> m (Element, a)
u_ = element (Proxy :: Proxy "u") "u"

var_ :: MonadBase Incremental m => m (ElementOptions "var")
  -> [Attribute "var"]
  -> m a
  -> m (Element, a)
var_ = element (Proxy :: Proxy "var") "var"

wbr_ :: MonadBase Incremental m => m (ElementOptions "wbr")
  -> [Attribute "wbr"]
  -> m Element
wbr_ = elementVoid (Proxy :: Proxy "wbr") "wbr"

area_ :: MonadBase Incremental m => m (ElementOptions "area")
  -> [Attribute "area"]
  -> m Element
area_ = elementVoid (Proxy :: Proxy "area") "area"

audio_ :: MonadBase Incremental m => m (ElementOptions "audio")
  -> [Attribute "audio"]
  -> m a
  -> m (Element, a)
audio_ = element (Proxy :: Proxy "audio") "audio"

map_ :: MonadBase Incremental m => m (ElementOptions "map")
  -> [Attribute "map"]
  -> m a
  -> m (Element, a)
map_ = element (Proxy :: Proxy "map") "map"

track_ :: MonadBase Incremental m => m (ElementOptions "track")
  -> [Attribute "track"]
  -> m Element
track_ = elementVoid (Proxy :: Proxy "track") "track"

video_ :: MonadBase Incremental m => m (ElementOptions "video")
  -> [Attribute "video"]
  -> m a
  -> m (Element, a)
video_ = element (Proxy :: Proxy "video") "video"

embed_ :: MonadBase Incremental m => m (ElementOptions "embed")
  -> [Attribute "embed"]
  -> m Element
embed_ = elementVoid (Proxy :: Proxy "embed") "embed"

object_ :: MonadBase Incremental m => m (ElementOptions "object")
  -> [Attribute "object"]
  -> m a
  -> m (Element, a)
object_ = element (Proxy :: Proxy "object") "object"

param_ :: MonadBase Incremental m => m (ElementOptions "param")
  -> [Attribute "param"]
  -> m Element
param_ = elementVoid (Proxy :: Proxy "param") "param"

source_ :: MonadBase Incremental m => m (ElementOptions "source")
  -> [Attribute "source"]
  -> m Element
source_ = elementVoid (Proxy :: Proxy "source") "source"

canvas_ :: MonadBase Incremental m => m (ElementOptions "canvas")
  -> [Attribute "canvas"]
  -> m a
  -> m (Element, a)
canvas_ = element (Proxy :: Proxy "canvas") "canvas"

noscript_ :: MonadBase Incremental m => m (ElementOptions "noscript")
  -> [Attribute "noscript"]
  -> m a
  -> m (Element, a)
noscript_ = element (Proxy :: Proxy "noscript") "noscript"

script_ :: MonadBase Incremental m => m (ElementOptions "script")
  -> [Attribute "script"]
  -> m a
  -> m (Element, a)
script_ = element (Proxy :: Proxy "script") "script"

del_ :: MonadBase Incremental m => m (ElementOptions "del")
  -> [Attribute "del"]
  -> m a
  -> m (Element, a)
del_ = element (Proxy :: Proxy "del") "del"

ins_ :: MonadBase Incremental m => m (ElementOptions "ins")
  -> [Attribute "ins"]
  -> m a
  -> m (Element, a)
ins_ = element (Proxy :: Proxy "ins") "ins"

caption_ :: MonadBase Incremental m => m (ElementOptions "caption")
  -> [Attribute "caption"]
  -> m a
  -> m (Element, a)
caption_ = element (Proxy :: Proxy "caption") "caption"

col_ :: MonadBase Incremental m => m (ElementOptions "col")
  -> [Attribute "col"]
  -> m Element
col_ = elementVoid (Proxy :: Proxy "col") "col"

command_ :: MonadBase Incremental m => m (ElementOptions "command")
  -> [Attribute "command"]
  -> m Element
command_ = elementVoid (Proxy :: Proxy "command") "command"

colgroup_ :: MonadBase Incremental m => m (ElementOptions "colgroup")
  -> [Attribute "colgroup"]
  -> m a
  -> m (Element, a)
colgroup_ = element (Proxy :: Proxy "colgroup") "colgroup"

table_ :: MonadBase Incremental m => m (ElementOptions "table")
  -> [Attribute "table"]
  -> m a
  -> m (Element, a)
table_ = element (Proxy :: Proxy "table") "table"

tbody_ :: MonadBase Incremental m => m (ElementOptions "tbody")
  -> [Attribute "tbody"]
  -> m a
  -> m (Element, a)
tbody_ = element (Proxy :: Proxy "tbody") "tbody"

td_ :: MonadBase Incremental m => m (ElementOptions "td")
  -> [Attribute "td"]
  -> m a
  -> m (Element, a)
td_ = element (Proxy :: Proxy "td") "td"

tfoot_ :: MonadBase Incremental m => m (ElementOptions "tfoot")
  -> [Attribute "tfoot"]
  -> m a
  -> m (Element, a)
tfoot_ = element (Proxy :: Proxy "tfoot") "tfoot"

th_ :: MonadBase Incremental m => m (ElementOptions "th")
  -> [Attribute "th"]
  -> m a
  -> m (Element, a)
th_ = element (Proxy :: Proxy "th") "th"

thead_ :: MonadBase Incremental m => m (ElementOptions "thead")
  -> [Attribute "thead"]
  -> m a
  -> m (Element, a)
thead_ = element (Proxy :: Proxy "thead") "thead"

tr_ :: MonadBase Incremental m => m (ElementOptions "tr")
  -> [Attribute "tr"]
  -> m a
  -> m (Element, a)
tr_ = element (Proxy :: Proxy "tr") "tr"

button_ :: MonadBase Incremental m => m (ElementOptions "button")
  -> [Attribute "button"]
  -> m a
  -> m (Element, a)
button_ = element (Proxy :: Proxy "button") "button"

datalist_ :: MonadBase Incremental m => m (ElementOptions "datalist")
  -> [Attribute "datalist"]
  -> m a
  -> m (Element, a)
datalist_ = element (Proxy :: Proxy "datalist") "datalist"

fieldset_ :: MonadBase Incremental m => m (ElementOptions "fieldset")
  -> [Attribute "fieldset"]
  -> m a
  -> m (Element, a)
fieldset_ = element (Proxy :: Proxy "fieldset") "fieldset"

form_ :: MonadBase Incremental m => m (ElementOptions "form")
  -> [Attribute "form"]
  -> m a
  -> m (Element, a)
form_ = element (Proxy :: Proxy "form") "form"

input_ :: MonadBase Incremental m => m (ElementOptions "input")
  -> [Attribute "input"]
  -> m Element
input_ = elementVoid (Proxy :: Proxy "input") "input"

label_ :: MonadBase Incremental m => m (ElementOptions "label")
  -> [Attribute "label"]
  -> m a
  -> m (Element, a)
label_ = element (Proxy :: Proxy "label") "label"

legend_ :: MonadBase Incremental m => m (ElementOptions "legend")
  -> [Attribute "legend"]
  -> m a
  -> m (Element, a)
legend_ = element (Proxy :: Proxy "legend") "legend"

meter_ :: MonadBase Incremental m => m (ElementOptions "meter")
  -> [Attribute "meter"]
  -> m a
  -> m (Element, a)
meter_ = element (Proxy :: Proxy "meter") "meter"

optgroup_ :: MonadBase Incremental m => m (ElementOptions "optgroup")
  -> [Attribute "optgroup"]
  -> m a
  -> m (Element, a)
optgroup_ = element (Proxy :: Proxy "optgroup") "optgroup"

option_ :: MonadBase Incremental m => m (ElementOptions "option")
  -> [Attribute "option"]
  -> m a
  -> m (Element, a)
option_ = element (Proxy :: Proxy "option") "option"

output_ :: MonadBase Incremental m => m (ElementOptions "output")
  -> [Attribute "output"]
  -> m a
  -> m (Element, a)
output_ = element (Proxy :: Proxy "output") "output"

progress_ :: MonadBase Incremental m => m (ElementOptions "progress")
  -> [Attribute "progress"]
  -> m a
  -> m (Element, a)
progress_ = element (Proxy :: Proxy "progress") "progress"

select_ :: MonadBase Incremental m => m (ElementOptions "select")
  -> [Attribute "select"]
  -> m a
  -> m (Element, a)
select_ = element (Proxy :: Proxy "select") "select"

details_ :: MonadBase Incremental m => m (ElementOptions "details")
  -> [Attribute "details"]
  -> m a
  -> m (Element, a)
details_ = element (Proxy :: Proxy "details") "details"

dialog_ :: MonadBase Incremental m => m (ElementOptions "dialog")
  -> [Attribute "dialog"]
  -> m a
  -> m (Element, a)
dialog_ = element (Proxy :: Proxy "dialog") "dialog"

menu_ :: MonadBase Incremental m => m (ElementOptions "menu")
  -> [Attribute "menu"]
  -> m a
  -> m (Element, a)
menu_ = element (Proxy :: Proxy "menu") "menu"

menuitem_ :: MonadBase Incremental m => m (ElementOptions "menuitem")
  -> [Attribute "menuitem"]
  -> m a
  -> m (Element, a)
menuitem_ = element (Proxy :: Proxy "menuitem") "menuitem"

summary_ :: MonadBase Incremental m => m (ElementOptions "summary")
  -> [Attribute "summary"]
  -> m a
  -> m (Element, a)
summary_ = element (Proxy :: Proxy "summary") "summary"

content_ :: MonadBase Incremental m => m (ElementOptions "content")
  -> [Attribute "content"]
  -> m a
  -> m (Element, a)
content_ = element (Proxy :: Proxy "content") "content"

decorator_ :: MonadBase Incremental m => m (ElementOptions "decorator")
  -> [Attribute "decorator"]
  -> m a
  -> m (Element, a)
decorator_ = element (Proxy :: Proxy "decorator") "decorator"

element_ :: MonadBase Incremental m => m (ElementOptions "element")
  -> [Attribute "element"]
  -> m a
  -> m (Element, a)
element_ = element (Proxy :: Proxy "element") "element"

shadow_ :: MonadBase Incremental m => m (ElementOptions "shadow")
  -> [Attribute "shadow"]
  -> m a
  -> m (Element, a)
shadow_ = element (Proxy :: Proxy "shadow") "shadow"

template_ :: MonadBase Incremental m => m (ElementOptions "template")
  -> [Attribute "template"]
  -> m a
  -> m (Element, a)
template_ = element (Proxy :: Proxy "template") "template"

link_ :: MonadBase Incremental m => m (ElementOptions "link")
  -> [Attribute "link"]
  -> m Element
link_ = elementVoid (Proxy :: Proxy "link") "link"

meta_ :: MonadBase Incremental m => m (ElementOptions "meta")
  -> [Attribute "meta"]
  -> m Element
meta_ = elementVoid (Proxy :: Proxy "meta") "meta"


{-
 - deprecated elements
acronym
applet
basefont
big
blink
center
dir
frame
frameset
isindex
listing
noembed
plaintext
spacer
strike
tt
xmp
-}
