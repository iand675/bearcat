{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Attributes where
import Data.JSString
import Data.Proxy
import GHC.TypeLits
import GHCJS.Marshal
import GHCJS.Types
import System.IO.Unsafe
import Unsafe.Coerce

type Text = String

data Attribute (e :: Symbol) = Attribute {-# UNPACK #-} !JSString
                                         {-# UNPACK #-} !JSVal

{-
type Attribute v e = Attribute e

data AttributeValidity = Valid | Invalid

type family ValidFor ys x where
  ValidFor '[]       x = 'Invalid
  ValidFor (x ': ys) x = 'Valid
  ValidFor (y ': ys) x = ValidFor ys x

type GlobalAttribute = Attribute 'Valid
type ValidAttribute l e = Attribute (ValidFor l e) e
-}

-- | Force an attribute to validate for an element that it shouldn't according to the HTML5 spec.
-- Useful for custom elements.
force :: Proxy e' -> Attribute e' -> Attribute e
force _ (Attribute k v) = Attribute k v

-- | Create an arbitrary attribute
attribute :: ToJSVal a => JSString -> a -> Attribute e
attribute str = Attribute str . unsafePerformIO . toJSVal

-- | Create an HTML5 data attribute
custom :: JSString -> JSVal -> Attribute e
custom str = Attribute (append "data-" str)

bool :: Bool -> JSVal
bool x = if x then jsval ("" :: JSString) else nullRef

emptyStr :: JSString
emptyStr = ""

class ValidAttr (attr :: Symbol) (elem :: Symbol) where

class AttrInput (attr :: Symbol) (elem :: Symbol) input | attr elem -> input where

-- | Convenience function for toggling empty-content attributes
toggleable :: Bool -> Attribute e -> Attribute e
toggleable switch attr@(Attribute name _) = if switch then attr else Attribute name nullRef

-- | If the value of the type attribute is file, this attribute indicates the types of files that the server accepts; otherwise it is ignored. The value must be a comma-separated list of unique content type specifiers:
--
--   * A file extension starting with the STOP character (U+002E). (E.g.: ".jpg,.png,.doc")
--   * A valid MIME type with no extensions
--   * audio/* representing sound files HTML5
--   * video/* representing video files HTML5
--   * image/* representing image files HTML5
accept_ :: ValidAttr "accept" e => Text -> Attribute e
accept_ = attribute "accept"

instance ValidAttr "accept" "form"
instance ValidAttr "accept" "input"

-- | A space- or comma-delimited list of character encodings that the server accepts. The browser uses them in the order in which they are listed. The default value, the reserved string "UNKNOWN", indicates the same encoding as that of the document containing the form element.
--
-- In previous versions of HTML, the different character encodings could be delimited by spaces or commas. In HTML5, only spaces are allowed as delimiters.
acceptCharset_ :: ValidAttr "accept-charset" e => Text -> Attribute e
acceptCharset_ = attribute "accept-charset"

instance ValidAttr "accept-charset" "form"

-- | Provides a hint for generating a keyboard shortcut for the current element. This attribute consists of a space-separated list of characters. The browser should use the first one that exists on the computer keyboard layout.
accessKey_ :: Text -> Attribute e
accessKey_ = attribute "accesskey"

-- | The URI of a program that processes the form information. This value can be overridden by a formaction attribute on a <button> or <input> element.
action_ :: Text -> Attribute e
action_ = attribute "action"

instance ValidAttr "action" "form"

-- | This attribute defines the alternative text describing the image. Users will see this displayed if the image URL is wrong, the image is not in one of the supported formats, or if the image is not yet downloaded.
alt_ :: ValidAttr "alt" e => Text -> Attribute e
alt_ = attribute "alt"

instance ValidAttr "alt" "area"
instance ValidAttr "alt" "img"
instance ValidAttr "alt" "input"

-- | Set this Boolean attribute to indicate that the browser should, if possible, execute the script asynchronously. It has no effect on inline scripts (i.e., scripts that don't have the src attribute).
async_ :: ValidAttr "async" e => Attribute e
async_ = attribute "async" emptyStr

instance ValidAttr "async" "script"

-- | This attribute lets you specify that a form control or button should have input focus when the page loads, unless the user overrides it, for example by typing in a different control. Only one form element in a document can have the autofocus attribute. It cannot be applied if the type attribute is set to hidden (that is, you cannot automatically set focus to a hidden control). Note that the focusing of the control may occur before the firing of the DOMContentLoaded event.
autofocus_ :: ValidAttr "autofocus" e => Attribute e
autofocus_ = attribute "autofocus" emptyStr

instance ValidAttr "autofocus" "button"
instance ValidAttr "autofocus" "input"
instance ValidAttr "autofocus" "select"
instance ValidAttr "autofocus" "textarea"

-- | The audio or video should play as soon as possible.
autoplay_ :: ValidAttr "autoplay" e => Attribute  e
autoplay_ = attribute "autoplay" emptyStr

instance ValidAttr "autoplay" "audio"
instance ValidAttr "autoplay" "video"

-- | Previous values should persist dropdowns of selectable values across page loads.
autosave_ :: ValidAttr "autosave" e => Attribute e
autosave_ = attribute "autosave" emptyStr

instance ValidAttr "autosave" "input"

{-
-- | Contains the time range of already buffered media.
buffered_ :: Bool ->["audio", "video"]
buffered_ = attribute "buffered" emptyStr

instance ValidAttr "buffered" "audio"
instance ValidAttr "buffered" "video"
-}

-- | Declares the character encoding of the page or script.
charset_ :: ValidAttr "charset" e => Text -> Attribute e
charset_ = attribute "charset"

instance ValidAttr "charset" "meta"
instance ValidAttr "charset" "script"

-- | Indicates whether the element should be checked on page load.
checked_ :: ValidAttr "checked" e => Attribute e
checked_ = attribute "checked" emptyStr

instance ValidAttr "checked" "command"
instance ValidAttr "checked" "input"

-- | Contains a URI which points to the source of the quote or change.
cite_ :: ValidAttr "cite" e => Text -> Attribute e
cite_ = attribute "cite"

instance ValidAttr "cite" "blockquote"
instance ValidAttr "cite" "del"
instance ValidAttr "cite" "ins"
instance ValidAttr "cite" "q"

-- | Often used with CSS to style elements with common properties.
class_ :: Text -> Attribute e
class_ = attribute "class"

-- | The visible width of the text control, in average character widths. If it is specified, it must be a positive integer. If it is not specified, the default value is 20.
cols_ :: ValidAttr "cols" e => Int -> Attribute e
cols_ = attribute "cols"

instance ValidAttr "cols" "textarea"

-- | This attribute contains a non-negative integer value that indicates on how many columns does the cell extend. Its default value is 1; if its value is set to 0, it does extend until the end of the <colgroup>, eventually implicitly defined, the cell belongs to. Values higher than 1000 are clipped down to 1000
colspan_ :: ValidAttr "colspan" e => Int -> Attribute e
colspan_ = attribute "colspan"

instance ValidAttr "colspan" "td"
instance ValidAttr "colspan" "th"

-- | This attribute gives the value associated with the http-equiv or name attribute, depending of the context.
content_ :: ValidAttr "content" e => Text -> Attribute e
content_ = attribute "content"

instance ValidAttr "content" "meta"

-- | Is an enumerated attribute indicating if the element should be editable by the user. If so, the browser modifies its widget to allow editing.
contentEditable_ :: Bool -> Attribute e
contentEditable_ = attribute "contenteditable" -- intentionally not using bool helper here


-- | Is the id of an <menu> to use as the contextual menu for this element.
contextMenu_ :: Text -> Attribute e
contextMenu_ = attribute "contextmenu"

-- | If this attribute is present, the browser will offer controls to allow the user to control playback, including volume, seeking, and pause/resume playback.
controls_ :: ValidAttr "controls" e => Attribute e
controls_ = attribute "controls" emptyStr

instance ValidAttr "controls" "audio"
instance ValidAttr "controls" "video"

-- | A set of values specifying the coordinates of the hot-spot region. The number and meaning of the values depend upon the value specified for the shape attribute. For a rect or rectangle shape, the coords value is two x,y pairs: left, top, right, and bottom. For a circle shape, the value is x,y,r where x,y is a pair specifying the center of the circle and r is a value for the radius. For a poly or polygon< shape, the value is a set of x,y pairs for each point in the polygon: x1,y1,x2,y2,x3,y3, and so on. In HTML4, the values are numbers of pixels or percentages, if a percent sign (%) is appended; in HTML5, the values are numbers of CSS pixels.
coords_ :: ValidAttr "coords" e => Text -> Attribute e
coords_ = attribute "coords"

instance ValidAttr "coords" "area"

-- | The address of the resource as a valid URL. At least one of data and type must be defined.
data_ :: ValidAttr "data" e => Text -> Attribute e
data_ = attribute "data"

instance ValidAttr "data" "object"

-- | Indicates the date and time associated with the element and must be a valid date with an optional time string.
datetime_ :: ValidAttr "datetime" e => Text -> Attribute  e
datetime_ = attribute "datetime"

instance ValidAttr "datetime" "del"
instance ValidAttr "datetime" "ins"
instance ValidAttr "datetime" "time"

-- | This attribute indicates that the track should be enabled unless the user's preferences indicate that another track is more appropriate.
-- This may only be used on one track element per media element.
default_ :: ValidAttr "default" e => Bool -> Attribute e
default_ = attribute "default"

instance ValidAttr "default" "track"

-- | Indicates that the script should be executed after the page has been parsed.
defer_ :: ValidAttr "defer" e => Bool -> Attribute e
defer_ = attribute "defer"

instance ValidAttr "defer" "script"

data TextDirection = LeftToRight | RightToLeft

-- | Defines the text direction.
dir_ :: TextDirection -> Attribute e
dir_ x = attribute "dir" $ case x of
  LeftToRight -> "ltr" :: JSString
  RightToLeft -> "rtl" :: JSString

-- | Once the form for the input is submitted, a parameter with the given dirname with value ltr or rtl will be appended to the URL.
dirname_ :: ValidAttr "dirname" e => Text -> Attribute e
dirname_ = attribute "dirname"

instance ValidAttr "dirname" "input"
instance ValidAttr "dirname" "textarea"

-- | Indicates whether the user can interact with the element.
disabled_ :: ValidAttr "disabled" e => Attribute e
disabled_ = attribute "disabled" emptyStr

instance ValidAttr "disabled" "button"
instance ValidAttr "disabled" "command"
instance ValidAttr "disabled" "fieldset"
instance ValidAttr "disabled" "input"
instance ValidAttr "disabled" "optgroup"
instance ValidAttr "disabled" "option"
instance ValidAttr "disabled" "select"
instance ValidAttr "disabled" "textarea"

-- | Indicates that the hyperlink is to be used for downloading a resource.
download_ :: ValidAttr "download" e => Attribute e
download_ = attribute "download" emptyStr

instance ValidAttr "download" "a"
instance ValidAttr "download" "area"

-- | Defines whether the element can be dragged.
draggable_ :: Attribute e
draggable_ = attribute "draggable" emptyStr

-- | Indicates that the element accepts the dropping of content on it.
dropzone_ :: Attribute e
dropzone_ = attribute "dropzone" emptyStr

-- | Defines the content type of the form data when the method is POST.
encType_ :: ValidAttr "enctype" e => Text -> Attribute e
encType_ = attribute "enctype"

instance ValidAttr "enctype" "form"

-- | Describes elements which belongs to this one.
for_ :: ValidAttr "for" e => Text -> Attribute e
for_ = attribute "for"

instance ValidAttr "for" "label"
instance ValidAttr "for" "output"

-- | Indicates the form that is the owner of the element. The value of the attribute must be an ID of a <form> element in the same document.
form_ :: ValidAttr "form" e => Text -> Attribute e
form_ = attribute "form"

instance ValidAttr "form" "button"
instance ValidAttr "form" "fieldset"
instance ValidAttr "form" "input"
instance ValidAttr "form" "label"
instance ValidAttr "form" "meter"
instance ValidAttr "form" "object"
instance ValidAttr "form" "output"
instance ValidAttr "form" "progress"
instance ValidAttr "form" "select"
instance ValidAttr "form" "textarea"

-- | The URI of a program that processes the information submitted by the button or input.
-- If specified, it overrides the action attribute of the element's form owner.
formAction_ :: ValidAttr "formaction" e => Text -> Attribute e
formAction_ = attribute "formaction"

instance ValidAttr "formaction" "input"
instance ValidAttr "formaction" "button"

-- | IDs of the <th> elements which applies to this element.
headers_ :: ValidAttr "headers" e => Text -> Attribute e
headers_ = attribute "headers"

instance ValidAttr "headers" "td"
instance ValidAttr "headers" "th"

-- | The intrinsic height of the element in HTML5 CSS pixels
height_ :: ValidAttr "height" e => Int -> Attribute  e
height_ = attribute "height"

instance ValidAttr "height" "canvas"
instance ValidAttr "height" "embed"
instance ValidAttr "height" "iframe"
instance ValidAttr "height" "img"
instance ValidAttr "height" "input"
instance ValidAttr "height" "object"
instance ValidAttr "height" "video"

-- | Prevents rendering of given element, while keeping child elements, e.g. script elements, active.
hidden_ :: Attribute e
hidden_ = attribute "hidden" emptyStr

-- | The lower numeric bound of the high end of the measured range. This must be less than the maximum value (max attribute), and it also must be greater than the low value and minimum value (low attribute and min attribute, respectively), if any are specified. If unspecified, or if greater than the maximum value, the high value is equal to the maximum value.
high_ :: ValidAttr "high" e => Double -> Attribute e
high_ = attribute "high"

instance ValidAttr "high" "meter"

-- | The URL of a linked resource.
href_ :: ValidAttr "href" e => Text -> Attribute e
href_ = attribute "href"

instance ValidAttr "href" "a"
instance ValidAttr "href" "area"
instance ValidAttr "href" "base"
instance ValidAttr "href" "link"

-- | Specifies the language of the linked resource.
hrefLang_ :: ValidAttr "hreflang" e => Text -> Attribute e
hrefLang_ = attribute "hreflang"

instance ValidAttr "hreflang" "a"
instance ValidAttr "hreflang" "area"
instance ValidAttr "hreflang" "link"

-- | TODO, a bit complicated to encode into the type system
httpEquiv_ :: ValidAttr "http-equiv" e => Text -> Attribute e
httpEquiv_ = attribute "http-equiv"

instance ValidAttr "http-equiv" "meta"

-- | Specifies a picture which represents the command.
icon_ :: ValidAttr "icon" e => Text -> Attribute e
icon_ = attribute "icon"

instance ValidAttr "icon" "command"

-- | Often used with CSS to style a specific element. The value of this attribute must be unique.
id_ :: Text -> Attribute e
id_ = attribute "id"

-- | Indicates that the image is part of a server-side image map.
isMap_ :: ValidAttr "ismap" e => Attribute e
isMap_ = attribute "ismap" emptyStr

instance ValidAttr "ismap" "img"

-- | Part of the microdata spec. Used to label semantic properties of an item.
itemProp_ :: Text -> Attribute e
itemProp_ = attribute "itemprop"

{-
kind_ :: Attribute (ValidFor ["track"] e) e
kind_ = attribute "kind"

instance ValidAttr

label_ :: Attribute (ValidFor ["track"] e) e
label_ = attribute "label"

instance ValidAttr

lang_

language_ :: Text -> Attribute (ValidFor '["script"] e) e
language_ = attribute "language"

instance ValidAttr

list_ :: Text -> Attribute (ValidFor '["input"] e) e
list_ = attribute "list"

instance ValidAttr

loop_ :: Attribute (ValidFor '["audio", "bgsound", "marquee", "video"] e) e
loop_ = attribute "loop"

instance ValidAttr
instance ValidAttr
instance ValidAttr
instance ValidAttr

low_ :: Attribute (ValidFor '["meter"] e) e
low_ = attribute "low"

instance ValidAttr

manifest_ :: Attribute (ValidFor ["html"] e) e
manifest_ = attribute "manifest"

instance ValidAttr
-}

-- | Indicates the maximum value allowed.
max_ :: ValidAttr "max" e => Double -> Attribute e
max_ = attribute "max"

instance ValidAttr "max" "input"
instance ValidAttr "max" "meter"
instance ValidAttr "max" "progress"

-- | Defines the maximum number of characters allowed in the element.
maxLength_ :: ValidAttr "maxlength" e => Int -> Attribute e
maxLength_ = attribute "maxlength"

instance ValidAttr "maxlength" "input"
instance ValidAttr "maxlength" "textarea"

-- | Specifies a hint of the media for which the linked resource was designed.
media_ :: ValidAttr "media" e => Text -> Attribute e
media_ = attribute "media"

instance ValidAttr "media" "a"
instance ValidAttr "media" "area"
instance ValidAttr "media" "link"
instance ValidAttr "media" "source"
instance ValidAttr "media" "style"

data FormMethod = FormGet | FormPost

-- | Defines which HTTP method to use when submitting the form. Can be GET (default) or POST.
method_ :: ValidAttr "method" e => FormMethod -> Attribute e
method_ x = attribute "method" $ case x of
  FormGet  -> "get" :: JSString
  FormPost -> "post" :: JSString

instance ValidAttr "method" "form"

-- | Indicates the minimum value allowed.
min_ :: ValidAttr "min" e => Double -> Attribute e
min_ = attribute "min"

instance ValidAttr "min" "input"
instance ValidAttr "min" "meter"

-- | This attribute indicates that multiple options can be selected in the list.
-- If it is not specified, then only one option can be selected at a time.
multiple_ :: ValidAttr "multiple" e => Attribute e
multiple_ = attribute "multiple" emptyStr

instance ValidAttr "multiple" "input"
instance ValidAttr "multiple" "select"

-- | Name of the element. For example used by the server to identify the fields in form submits.
name_ :: ValidAttr "name" e => Text -> Attribute e
name_ = attribute "name"

instance ValidAttr "name" "button"
instance ValidAttr "name" "form"
instance ValidAttr "name" "fieldset"
instance ValidAttr "name" "iframe"
instance ValidAttr "name" "input"
instance ValidAttr "name" "object"
instance ValidAttr "name" "output"
instance ValidAttr "name" "select"
instance ValidAttr "name" "textarea"
instance ValidAttr "name" "map"
instance ValidAttr "name" "meta"
instance ValidAttr "name" "param"

-- | This attribute indicates that the form shouldn't be validated when submitted.
noValidate_ :: ValidAttr "novalidate" e => Attribute e
noValidate_ = attribute "novalidate" emptyStr

instance ValidAttr "novalidate" "form"

-- | Indicates whether the details will be shown on page load.
open_ :: ValidAttr "open" e => Attribute e
open_ = attribute "open" emptyStr

instance ValidAttr "open" "details"

-- | Indicates the optimal numeric value.
optimum_ :: ValidAttr "optimum" e => Double -> Attribute e
optimum_ = attribute "optimum"

instance ValidAttr "optimium" "meter"

-- | Defines a regular expression which the element's value will be validated against.
pattern_ :: ValidAttr "pattern" e => Text -> Attribute e
pattern_ = attribute "pattern"

instance ValidAttr "pattern" "input"

-- | Sends the URLs of the resources a notification/ping if the user follows the hyperlink.
ping_ :: ValidAttr "ping" e => Text -> Attribute e
ping_ = attribute "ping"

instance ValidAttr "ping" "a"
instance ValidAttr "ping" "area"

-- | Provides a hint to the user of what can be entered in the field.
placeholder_ :: ValidAttr "placeholder" e => Text -> Attribute e
placeholder_ = attribute "placeholder"

instance ValidAttr "placeholder" "input"
instance ValidAttr "placeholder" "textarea"

{-
poster_ :: Attribute (ValidFor '["video"] e) e
poster_ = attribute "poster"

instance ValidAttr
-}

-- | Indicates whether the whole resource, parts of it or nothing should be preloaded.
preload_ :: ValidAttr "preload" e => Attribute e
preload_ = attribute "preload" emptyStr

instance ValidAttr "preload" "audio"
instance ValidAttr "preload" "video"

{-
pubdate_ :: Attribute (ValidFor '["time"] e) e
pubdate_ = attribute "pubdate"

instance ValidAttr
-}

radiogroup_ :: ValidAttr "radiogroup" e => Text -> Attribute e
radiogroup_ = attribute "radiogroup"

instance ValidAttr "radiogroup" "command"

-- | Indicates whether the element can be edited.
readOnly_ :: ValidAttr "readonly" e => Attribute e
readOnly_ = attribute "readonly" emptyStr

instance ValidAttr "readonly" "input"
instance ValidAttr "readonly" "textarea"

-- | Specifies the relationship of the target object to the link object.
rel_ :: ValidAttr "rel" e => Text -> Attribute e
rel_ = attribute "rel"

instance ValidAttr "rel" "a"
instance ValidAttr "rel" "area"
instance ValidAttr "rel" "link"

-- | Indicates whether this element is required to fill out or not.
required_ :: ValidAttr "required" e => Attribute e
required_ = attribute "required" emptyStr

instance ValidAttr "required" "input"
instance ValidAttr "required" "select"
instance ValidAttr "required" "textarea"

-- | Indicates whether the list should be displayed in a descending order instead of a ascending.
reversed_ :: ValidAttr "reversed" e => Attribute e
reversed_ = attribute "reversed" emptyStr

instance ValidAttr "reversed" "ol"

-- | Defines the number of rows in a text area.
rows_ :: ValidAttr "rows" e => Int -> Attribute e
rows_ = attribute "rows"

instance ValidAttr "rows" "textarea"

{-
rowspan_ ["td", "th"]

instance ValidAttr
instance ValidAttr

sandbox_ ["iframe"]

instance ValidAttr

scope_ ["th"]

instance ValidAttr
-}

-- | If this attribute is present, then the style applies only to its parent element. If absent, the style applies to the whole document.
scoped_ :: ValidAttr "scoped" e => Attribute e
scoped_ = attribute "scoped" emptyStr

instance ValidAttr "scoped" "style"

-- | This attribute indicates that the browser should render the inline frame in a way that makes it appear to be part of the containing document, for example by applying CSS styles that apply to the <iframe> to the contained document before styles specified in that document, and by opening links in the contained documents in the parent browsing context (unless another setting prevents this).
seamless_ :: ValidAttr "seamless" e => Attribute e
seamless_ = attribute "seamless" emptyStr

instance ValidAttr "seamless" "iframe"

-- | Defines a value which will be selected on page load. If the <option> element is the descendant of a <select>
-- element whose multiple attribute is not set, only one single <option> of this <select> element may have the selected attribute.
selected_ :: ValidAttr "selected" e => Attribute e
selected_ = attribute "selected" emptyStr

instance ValidAttr "selected" "option"

{-
shape_ ["a", "area"]
size_ ["input", "select"]
sizes_ ["link", "img", "source"]
span_ ["col", "colgroup"]
-}

-- | Indicates whether spell checking is allowed for the element.
spellcheck_ :: Bool -> Attribute e
spellcheck_ = attribute "spellcheck"

-- | The URL of the embeddable content.
src_ :: ValidAttr "src" e => Text -> Attribute e
src_ = attribute "src"

instance ValidAttr "src" "audio"
instance ValidAttr "src" "embed"
instance ValidAttr "src" "iframe"
instance ValidAttr "src" "img"
instance ValidAttr "src" "input"
instance ValidAttr "src" "script"
instance ValidAttr "src" "source"
instance ValidAttr "src" "track"
instance ValidAttr "src" "video"

-- | The content of the page that the embedded context is to contain. This attribute is expected to be used together with the sandbox and seamless attributes. If a browser supports the srcdoc attribute, it will override the content specified in the src attribute (if present). If a browser does NOT support the srcdoc attribute, it will show the file specified in the src attribute instead (if present).
srcDoc_ :: ValidAttr "srcdoc" e => Text -> Attribute e
srcDoc_ = attribute "srcdoc"

instance ValidAttr "srcdoc" "iframe"

{-
srcLang_ ["track"]

instance ValidAttr

srcSet_ ["img"]
instance ValidAttr

start_ ["ol"]
instance ValidAttr

step_ ["input"]
instance ValidAttr
-}

-- | Defines CSS styles which will override styles previously set.
--
-- TODO, make a ToStyle class or the like
style_ :: Text -> Attribute e
style_ = attribute "style"

-- | Overrides the browser's default tab order and follows the one specified instead.
tabIndex_ :: Int -> Attribute e
tabIndex_ = attribute "tabindex"

-- | This attribute specifies where to display the linked resource. In HTML4, this is the name of, or a keyword for, a frame. In HTML5, it is a name of, or keyword for, a browsing context (for example, tab, window, or inline frame).
target_ :: ValidAttr "target" e => Text -> Attribute e
target_ = attribute "target"

instance ValidAttr "target" "a"
instance ValidAttr "target" "area"
instance ValidAttr "target" "base"
instance ValidAttr "target" "form"

-- | Text to be displayed in a tooltip when hovering over the element.
title_ :: Text -> Attribute e
title_ = attribute "title"

-- | Defines the type of the element.
type_ :: ValidAttr "type" e => Text -> Attribute e
type_ = attribute "type"

instance ValidAttr "type" "button"
instance ValidAttr "type" "input"
instance ValidAttr "type" "command"
instance ValidAttr "type" "embed"
instance ValidAttr "type" "object"
instance ValidAttr "type" "script"
instance ValidAttr "type" "source"
instance ValidAttr "type" "style"
instance ValidAttr "type" "menu"

-- | Defines a default value which will be displayed in the element on page load.
value_ :: ValidAttr "value" e => Text -> Attribute e
value_ = attribute "value"

instance ValidAttr "value" "button"
instance ValidAttr "value" "option"
instance ValidAttr "value" "input"
instance ValidAttr "value" "li"
instance ValidAttr "value" "meter"
instance ValidAttr "value" "progress"
instance ValidAttr "value" "param"

-- | The intrinsic width of the element in HTML5 CSS pixels
width_ :: ValidAttr "width" e => Int -> Attribute e
width_ = attribute "width"

instance ValidAttr "width" "canvas"
instance ValidAttr "width" "embed"
instance ValidAttr "width" "iframe"
instance ValidAttr "width" "img"
instance ValidAttr "width" "input"
instance ValidAttr "width" "object"
instance ValidAttr "width" "video"

-- | Indicates whether the text should be wrapped.
wrap_ :: ValidAttr "wrap" e => Attribute e
wrap_ = attribute "wrap" emptyStr

instance ValidAttr "wrap" "textarea"
