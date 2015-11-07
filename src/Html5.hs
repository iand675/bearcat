{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Html5 where
import Data.Proxy
import Lib

base_ = elementVoid (Proxy :: Proxy "base") "base"
head_ = element (Proxy :: Proxy "head") "head"
style_ = element (Proxy :: Proxy "style") "style"
title_ = element (Proxy :: Proxy "title") "title"
address_ = element (Proxy :: Proxy "address") "address"
article_ = element (Proxy :: Proxy "article") "article"
footer_ = element (Proxy :: Proxy "footer") "footer"
h1_ = element (Proxy :: Proxy "h1") "h1"
h2_ = element (Proxy :: Proxy "h2") "h2"
h3_ = element (Proxy :: Proxy "h3") "h3"
h4_ = element (Proxy :: Proxy "h4") "h4"
h5_ = element (Proxy :: Proxy "h5") "h5"
h6_ = element (Proxy :: Proxy "h6") "h6"
hgroup_ = element (Proxy :: Proxy "hgroup") "hgroup"
nav_ = element (Proxy :: Proxy "nav") "nav"
section_ = element (Proxy :: Proxy "section") "section"
dd_ = element (Proxy :: Proxy "dd") "dd"
div_ = element (Proxy :: Proxy "div") "div"
dl_ = element (Proxy :: Proxy "dl") "dl"
dt_ = element (Proxy :: Proxy "dt") "dt"
figcaption_ = element (Proxy :: Proxy "figcaption") "figcaption"
figure_ = element (Proxy :: Proxy "figure") "figure"
hr_ = elementVoid (Proxy :: Proxy "hr") "hr"
li_ = element (Proxy :: Proxy "li") "li"
img_ = elementVoid (Proxy :: Proxy "img") "img"
main_ = element (Proxy :: Proxy "main") "main"
ol_ = element (Proxy :: Proxy "ol") "ol"
p_ = element (Proxy :: Proxy "p") "p"
pre_ = element (Proxy :: Proxy "pre") "pre"
ul_ = element (Proxy :: Proxy "ul") "ul"
abbr_ = element (Proxy :: Proxy "abbr") "abbr"
b_ = element (Proxy :: Proxy "b") "b"
bdi_ = element (Proxy :: Proxy "bdi") "bdi"
bdo_ = element (Proxy :: Proxy "bdo") "bdo"
br_ = elementVoid (Proxy :: Proxy "br") "br"
cite_ = element (Proxy :: Proxy "cite") "cite"
code_ = element (Proxy :: Proxy "code") "code"
data_ = element (Proxy :: Proxy "data") "data"
dfn_ = element (Proxy :: Proxy "dfn") "dfn"
em_ = element (Proxy :: Proxy "em") "em"
i_ = element (Proxy :: Proxy "i") "i"
kbd_ = element (Proxy :: Proxy "kbd") "kbd"
mark_ = element (Proxy :: Proxy "mark") "mark"
q_ = element (Proxy :: Proxy "q") "q"
rp_ = element (Proxy :: Proxy "rp") "rp"
rt_ = element (Proxy :: Proxy "rt") "rt"
rtc_ = element (Proxy :: Proxy "rtc") "rtc"
ruby_ = element (Proxy :: Proxy "ruby") "ruby"
s_ = element (Proxy :: Proxy "s") "s"
samp_ = element (Proxy :: Proxy "samp") "samp"
small_ = element (Proxy :: Proxy "small") "small"
span_ = element (Proxy :: Proxy "span") "span"
strong_ = element (Proxy :: Proxy "strong") "strong"
sub_ = element (Proxy :: Proxy "sub") "sub"
sup_ = element (Proxy :: Proxy "sup") "sup"
time_ = element (Proxy :: Proxy "time") "time"
u_ = element (Proxy :: Proxy "u") "u"
var_ = element (Proxy :: Proxy "var") "var"
wbr_ = elementVoid (Proxy :: Proxy "wbr") "wbr"
area_ = elementVoid (Proxy :: Proxy "area") "area"
audio_ = element (Proxy :: Proxy "audio") "audio"
map_ = element (Proxy :: Proxy "map") "map"
track_ = elementVoid (Proxy :: Proxy "track") "track"
video_ = element (Proxy :: Proxy "video") "video"
embed_ = elementVoid (Proxy :: Proxy "embed") "embed"
object_ = element (Proxy :: Proxy "object") "object"
param_ = elementVoid (Proxy :: Proxy "param") "param"
source_ = elementVoid (Proxy :: Proxy "source") "source"
canvas_ = element (Proxy :: Proxy "canvas") "canvas"
noscript_ = element (Proxy :: Proxy "noscript") "noscript"
script_ = element (Proxy :: Proxy "script") "script"
del_ = element (Proxy :: Proxy "del") "del"
ins_ = element (Proxy :: Proxy "ins") "ins"
caption_ = element (Proxy :: Proxy "caption") "caption"
col_ = elementVoid (Proxy :: Proxy "col") "col"
command_ = elementVoid (Proxy :: Proxy "command") "command"
colgroup_ = element (Proxy :: Proxy "colgroup") "colgroup"
table_ = element (Proxy :: Proxy "table") "table"
tbody_ = element (Proxy :: Proxy "tbody") "tbody"
td_ = element (Proxy :: Proxy "td") "td"
tfoot_ = element (Proxy :: Proxy "tfoot") "tfoot"
th_ = element (Proxy :: Proxy "th") "th"
thead_ = element (Proxy :: Proxy "thead") "thead"
tr_ = element (Proxy :: Proxy "tr") "tr"
button_ = element (Proxy :: Proxy "button") "button"
datalist_ = element (Proxy :: Proxy "datalist") "datalist"
fieldset_ = element (Proxy :: Proxy "fieldset") "fieldset"
form_ = element (Proxy :: Proxy "form") "form"
input_ = elementVoid (Proxy :: Proxy "input") "input"
keygen_ = elementVoid (Proxy :: Proxy "keygen") "keygen"
label_ = element (Proxy :: Proxy "label") "label"
legend_ = element (Proxy :: Proxy "legend") "legend"
meter_ = element (Proxy :: Proxy "meter") "meter"
optgroup_ = element (Proxy :: Proxy "optgroup") "optgroup"
option_ = element (Proxy :: Proxy "option") "option"
output_ = element (Proxy :: Proxy "output") "output"
progress_ = element (Proxy :: Proxy "progress") "progress"
select_ = element (Proxy :: Proxy "select") "select"
details_ = element (Proxy :: Proxy "details") "details"
dialog_ = element (Proxy :: Proxy "dialog") "dialog"
menu_ = element (Proxy :: Proxy "menu") "menu"
menuitem_ = element (Proxy :: Proxy "menuitem") "menuitem"
summary_ = element (Proxy :: Proxy "summary") "summary"
content_ = element (Proxy :: Proxy "content") "content"
decorator_ = element (Proxy :: Proxy "decorator") "decorator"
element_ = element (Proxy :: Proxy "element") "element"
shadow_ = element (Proxy :: Proxy "shadow") "shadow"
template_ = element (Proxy :: Proxy "template") "template"
link_ = elementVoid (Proxy :: Proxy "link") "link"
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
