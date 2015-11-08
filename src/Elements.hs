{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Elements where
import Attributes (Attribute)
import Control.Monad.Base
import Data.Proxy
import GHCJS.DOM.Types (Element)
import Incremental

-- | The HTML Anchor Element (<a>) defines a hyperlink to a location on the same page or any other page on the Web.
a_ :: MonadBase Incremental m => m (ElementOptions "a")
  -> [Attribute "a"]
  -> m a
  -> m (Element, a)
a_ = element (Proxy :: Proxy "a") "a"

-- | The HTML <base> element specifies the base URL to use for all relative URLs contained within a document. There can be only one <base> element in a document.
base_ :: MonadBase Incremental m => m (ElementOptions "base")
  -> [Attribute "base"]
  -> m Element
base_ = elementVoid (Proxy :: Proxy "base") "base"

-- | The HTML <head> element provides general information (metadata) about the document, including its title and links to/definitions of scripts and style sheets.
head_ :: MonadBase Incremental m => m (ElementOptions "head")
  -> [Attribute "head"]
  -> m a
  -> m (Element, a)
head_ = element (Proxy :: Proxy "head") "head"

-- | The HTML <style> element contains style information for a document, or part of a document. By default, the style instructions written inside that element are expected to be CSS.
style_ :: MonadBase Incremental m => m (ElementOptions "style")
  -> [Attribute "style"]
  -> m a
  -> m (Element, a)
style_ = element (Proxy :: Proxy "style") "style"

-- | The HTML <title> element defines the title of the document, shown in a browser's title bar or on the page's tab. It can only contain text and any contained tags are not interpreted.
title_ :: MonadBase Incremental m => m (ElementOptions "title")
  -> [Attribute "title"]
  -> m a
  -> m (Element, a)
title_ = element (Proxy :: Proxy "title") "title"

-- | The HTML <address> element supplies contact information for its nearest <article> or <body> ancestor; in the latter case, it applies to the whole document.
address_ :: MonadBase Incremental m => m (ElementOptions "address")
  -> [Attribute "address"]
  -> m a
  -> m (Element, a)
address_ = element (Proxy :: Proxy "address") "address"

-- | The HTML <article> element represents a self-contained composition in a document, page, application, or site, which is intended to be independently distributable or reusable (e.g., in syndication). This could be a forum post, a magazine or newspaper article, a blog entry, an object, or any other independent item of content. Each <article> should be identified, typically by including a heading (<h1>-<h6> element) as a child of the <article> element.
article_ :: MonadBase Incremental m => m (ElementOptions "article")
  -> [Attribute "article"]
  -> m a
  -> m (Element, a)
article_ = element (Proxy :: Proxy "article") "article"

-- | The HTML <footer> element represents a footer for its nearest sectioning content or sectioning root element. A footer typically contains information about the author of the section, copyright data or links to related documents.
footer_ :: MonadBase Incremental m => m (ElementOptions "footer")
  -> [Attribute "footer"]
  -> m a
  -> m (Element, a)
footer_ = element (Proxy :: Proxy "footer") "footer"

-- | The HTML <header> element represents a group of introductory or navigational aids. It may contain some heading elements but also other elements like a logo, wrapped section's header, a search form, and so on.
header_ :: MonadBase Incremental m => m (ElementOptions "header")
  -> [Attribute "header"]
  -> m a
  -> m (Element, a)
header_ = element (Proxy :: Proxy "header") "header"

{- $headings
   Heading elements implement six levels of document headings, <h1> is the most important and <h6> is the least. A heading element briefly describes the topic of the section it introduces. Heading information may be used by user agents, for example, to construct a table of contents for a document automatically.
-}

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

-- The HTML <hgroup> Element (HTML Headings Group Element) represents the heading of a section. It defines a single title that participates in the outline of the document as the heading of the implicit or explicit section that it belongs to.
hgroup_ :: MonadBase Incremental m => m (ElementOptions "hgroup")
  -> [Attribute "hgroup"]
  -> m a
  -> m (Element, a)
hgroup_ = element (Proxy :: Proxy "hgroup") "hgroup"

-- | The HTML <nav> element (HTML Navigation Element) represents a section of a page that links to other pages or to parts within the page: a section with navigation links.
nav_ :: MonadBase Incremental m => m (ElementOptions "nav")
  -> [Attribute "nav"]
  -> m a
  -> m (Element, a)
nav_ = element (Proxy :: Proxy "nav") "nav"

-- | The HTML <section> element represents a generic section of a document, i.e., a thematic grouping of content, typically with a heading. Each <section> should be identified, typically by including a heading (<h1>-<h6> element) as a child of the <section> element.
section_ :: MonadBase Incremental m => m (ElementOptions "section")
  -> [Attribute "section"]
  -> m a
  -> m (Element, a)
section_ = element (Proxy :: Proxy "section") "section"

-- | The HTML <dd> element (HTML Description Element) indicates the description of a term in a description list (<dl>) element. This element can occur only as a child element of a definition list and it must follow a <dt> element.
dd_ :: MonadBase Incremental m => m (ElementOptions "dd")
  -> [Attribute "dd"]
  -> m a
  -> m (Element, a)
dd_ = element (Proxy :: Proxy "dd") "dd"

-- | The HTML <div> element (or HTML Document Division Element) is the generic container for flow content, which does not inherently represent anything. It can be used to group elements for styling purposes (using the class or id attributes), or because they share attribute values, such as lang. It should be used only when no other semantic element (such as <article> or <nav>) is appropriate.
div_ :: MonadBase Incremental m => m (ElementOptions "div")
  -> [Attribute "div"]
  -> m a
  -> m (Element, a)
div_ = element (Proxy :: Proxy "div") "div"

-- | The HTML <dl> element (or HTML Description List Element) encloses a list of pairs of terms and descriptions. Common uses for this element are to implement a glossary or to display metadata (a list of key-value pairs).
dl_ :: MonadBase Incremental m => m (ElementOptions "dl")
  -> [Attribute "dl"]
  -> m a
  -> m (Element, a)
dl_ = element (Proxy :: Proxy "dl") "dl"

-- | The HTML <dt> element (or HTML Definition Term Element) identifies a term in a definition list. This element can occur only as a child element of a <dl>. It is usually followed by a <dd> element; however, multiple <dt> elements in a row indicate several terms that are all defined by the immediate next <dd> element.
dt_ :: MonadBase Incremental m => m (ElementOptions "dt")
  -> [Attribute "dt"]
  -> m a
  -> m (Element, a)
dt_ = element (Proxy :: Proxy "dt") "dt"

-- | The HTML <figcaption> element represents a caption or a legend associated with a figure or an illustration described by the rest of the data of the <figure> element which is its immediate ancestor which means <figcaption> can be the first or last element inside a <figure> block. Also, the HTML Figcaption Element is optional; if not provided, then the parent figure element will have no caption.
figcaption_ :: MonadBase Incremental m => m (ElementOptions "figcaption")
  -> [Attribute "figcaption"]
  -> m a
  -> m (Element, a)
figcaption_ = element (Proxy :: Proxy "figcaption") "figcaption"

-- | The HTML <figure> element represents self-contained content, frequently with a caption (<figcaption>), and is typically referenced as a single unit. While it is related to the main flow, its position is independent of the main flow. Usually this is an image, an illustration, a diagram, a code snippet, or a schema that is referenced in the main text, but that can be moved to another page or to an appendix without affecting the main flow.
figure_ :: MonadBase Incremental m => m (ElementOptions "figure")
  -> [Attribute "figure"]
  -> m a
  -> m (Element, a)
figure_ = element (Proxy :: Proxy "figure") "figure"

-- | The HTML <hr> element represents a thematic break between paragraph-level elements (for example, a change of scene in a story, or a shift of topic with a section). In previous versions of HTML, it represented a horizontal rule. It may still be displayed as a horizontal rule in visual browsers, but is now defined in semantic terms, rather than presentational terms.
hr_ :: MonadBase Incremental m => m (ElementOptions "hr")
  -> [Attribute "hr"]
  -> m Element
hr_ = elementVoid (Proxy :: Proxy "hr") "hr"

-- | The HTML <li> element (or HTML List Item Element) is used to represent an item in a list. It must be contained in a parent element: an ordered list (<ol>), an unordered list (<ul>), or a menu (<menu>). In menus and unordered lists, list items are usually displayed using bullet points. In ordered lists, they are usually displayed with an ascending counter on the left, such as a number or letter.
li_ :: MonadBase Incremental m => m (ElementOptions "li")
  -> [Attribute "li"]
  -> m a
  -> m (Element, a)
li_ = element (Proxy :: Proxy "li") "li"

-- | TODO
img_ :: MonadBase Incremental m => m (ElementOptions "img")
  -> [Attribute "img"]
  -> m Element
img_ = elementVoid (Proxy :: Proxy "img") "img"

-- | The HTML <main> element represents the main content of  the <body> of a document or application. The main content area consists of content that is directly related to, or expands upon the central topic of a document or the central functionality of an application. This content should be unique to the document, excluding any content that is repeated across a set of documents such as sidebars, navigation links, copyright information, site logos, and search forms (unless the document's main function is as a search form).
main_ :: MonadBase Incremental m => m (ElementOptions "main")
  -> [Attribute "main"]
  -> m a
  -> m (Element, a)
main_ = element (Proxy :: Proxy "main") "main"

-- | The HTML <ol> Element (or HTML Ordered List Element) represents an ordered list of items. Typically, ordered-list items are displayed with a preceding numbering, which can be of any form, like numerals, letters or Romans numerals or even simple bullets. This numbered style is not defined in the HTML description of the page, but in its associated CSS, using the list-style-type property.
ol_ :: MonadBase Incremental m => m (ElementOptions "ol")
  -> [Attribute "ol"]
  -> m a
  -> m (Element, a)
ol_ = element (Proxy :: Proxy "ol") "ol"

-- | The HTML <p> element (or HTML Paragraph Element) represents a paragraph of text.
p_ :: MonadBase Incremental m => m (ElementOptions "p")
  -> [Attribute "p"]
  -> m a
  -> m (Element, a)
p_ = element (Proxy :: Proxy "p") "p"

-- | The HTML <pre> element (or HTML Preformatted Text) represents preformatted text. Text within this element is typically displayed in a non-proportional font exactly as it is laid out in the file. Whitespaces inside this element are displayed as typed.
pre_ :: MonadBase Incremental m => m (ElementOptions "pre")
  -> [Attribute "pre"]
  -> m a
  -> m (Element, a)
pre_ = element (Proxy :: Proxy "pre") "pre"

-- | The HTML <ul> element (or HTML Unordered List Element) represents an unordered list of items, namely a collection of items that do not have a numerical ordering, and their order in the list is meaningless. Typically, unordered-list items are displayed with a bullet, which can be of several forms, like a dot, a circle or a squared. The bullet style is not defined in the HTML description of the page, but in its associated CSS, using the list-style-type property.
ul_ :: MonadBase Incremental m => m (ElementOptions "ul")
  -> [Attribute "ul"]
  -> m a
  -> m (Element, a)
ul_ = element (Proxy :: Proxy "ul") "ul"

-- | The HTML <abbr> element (or HTML Abbreviation Element) represents an abbreviation and optionally provides a full description for it. If present, the title attribute must contain this full description and nothing else.
abbr_ :: MonadBase Incremental m => m (ElementOptions "abbr")
  -> [Attribute "abbr"]
  -> m a
  -> m (Element, a)
abbr_ = element (Proxy :: Proxy "abbr") "abbr"

-- | The HTML <b> Element represents a span of text stylistically different from normal text, without conveying any special importance or relevance. It is typically used for keywords in a summary, product names in a review, or other spans of text whose typical presentation would be boldfaced. Another example of its use is to mark the lead sentence of each paragraph of an article.
b_ :: MonadBase Incremental m => m (ElementOptions "b")
  -> [Attribute "b"]
  -> m a
  -> m (Element, a)
b_ = element (Proxy :: Proxy "b") "b"

-- | The HTML <bdi> Element (or Bi-Directional Isolation Element) isolates a span of text that might be formatted in a different direction from other text outside it.
bdi_ :: MonadBase Incremental m => m (ElementOptions "bdi")
  -> [Attribute "bdi"]
  -> m a
  -> m (Element, a)
bdi_ = element (Proxy :: Proxy "bdi") "bdi"

-- | The HTML <bdo> Element (or HTML bidirectional override element) is used to override the current directionality of text. It causes the directionality of the characters to be ignored in favor of the specified directionality.
bdo_ :: MonadBase Incremental m => m (ElementOptions "bdo")
  -> [Attribute "bdo"]
  -> m a
  -> m (Element, a)
bdo_ = element (Proxy :: Proxy "bdo") "bdo"

-- | The HTML element line break <br> produces a line break in text (carriage-return). It is useful for writing a poem or an address, where the division of lines is significant.
br_ :: MonadBase Incremental m => m (ElementOptions "br")
  -> [Attribute "br"]
  -> m Element
br_ = elementVoid (Proxy :: Proxy "br") "br"

-- | The HTML Citation Element (<cite>) represents a reference to a creative work. It must include the title of a work or a URL reference, which may be in an abbreviated form according to the conventions used for the addition of citation metadata.
cite_ :: MonadBase Incremental m => m (ElementOptions "cite")
  -> [Attribute "cite"]
  -> m a
  -> m (Element, a)
cite_ = element (Proxy :: Proxy "cite") "cite"

-- | Represents formatted code
code_ :: MonadBase Incremental m => m (ElementOptions "code")
  -> [Attribute "code"]
  -> m a
  -> m (Element, a)
code_ = element (Proxy :: Proxy "code") "code"

-- | The HTML <data> Element links a given content with a machine-readable translation. If the content is time- or date-related, the <time> must be used.
data_ :: MonadBase Incremental m => m (ElementOptions "data")
  -> [Attribute "data"]
  -> m a
  -> m (Element, a)
data_ = element (Proxy :: Proxy "data") "data"

-- | The HTML Definition Element (<dfn>) represents the defining instance of a term.
dfn_ :: MonadBase Incremental m => m (ElementOptions "dfn")
  -> [Attribute "dfn"]
  -> m a
  -> m (Element, a)
dfn_ = element (Proxy :: Proxy "dfn") "dfn"

-- | The HTML element emphasis  <em> marks text that has stress emphasis. The <em> element can be nested, with each level of nesting indicating a greater degree of emphasis.
em_ :: MonadBase Incremental m => m (ElementOptions "em")
  -> [Attribute "em"]
  -> m a
  -> m (Element, a)
em_ = element (Proxy :: Proxy "em") "em"

-- | The HTML <i> Element represents a range of text that is set off from the normal text for some reason, for example, technical terms, foreign language phrases, or fictional character thoughts. It is typically displayed in italic type.
i_ :: MonadBase Incremental m => m (ElementOptions "i")
  -> [Attribute "i"]
  -> m a
  -> m (Element, a)
i_ = element (Proxy :: Proxy "i") "i"

-- | The HTML Keyboard Input Element (<kbd>) represents user input and produces an inline element displayed in the browser's default monospace font.
kbd_ :: MonadBase Incremental m => m (ElementOptions "kbd")
  -> [Attribute "kbd"]
  -> m a
  -> m (Element, a)
kbd_ = element (Proxy :: Proxy "kbd") "kbd"

-- | The HTML Mark Element (<mark>) represents highlighted text, i.e., a run of text marked for reference purpose, due to its relevance in a particular context. For example it can be used in a page showing search results to highlight every instance of the searched-for word.
mark_ :: MonadBase Incremental m => m (ElementOptions "mark")
  -> [Attribute "mark"]
  -> m a
  -> m (Element, a)
mark_ = element (Proxy :: Proxy "mark") "mark"

-- | The HTML Quote Element (<q>) indicates that the enclosed text is a short inline quotation. This element is intended for short quotations that don't require paragraph breaks; for long quotations use <blockquote> element.
q_ :: MonadBase Incremental m => m (ElementOptions "q")
  -> [Attribute "q"]
  -> m a
  -> m (Element, a)
q_ = element (Proxy :: Proxy "q") "q"

-- | The HTML <rp> element is used to provide fall-back parenthesis for browsers non-supporting ruby annotations. Ruby annotations are for showing pronounciation of East Asian characters, like using Japanese furigana or Taiwainese bopomofo characters. The <rp> element is used in the case of lack of <ruby> element support its content has what should be displayed in order to indicate the presence of a ruby annotation, usually parentheses.
rp_ :: MonadBase Incremental m => m (ElementOptions "rp")
  -> [Attribute "rp"]
  -> m a
  -> m (Element, a)
rp_ = element (Proxy :: Proxy "rp") "rp"

-- | The HTML <rt> Element embraces pronunciation of characters presented in a ruby annotations, which are used to describe the pronunciation of East Asian characters. This element is always used inside a <ruby> element.
rt_ :: MonadBase Incremental m => m (ElementOptions "rt")
  -> [Attribute "rt"]
  -> m a
  -> m (Element, a)
rt_ = element (Proxy :: Proxy "rt") "rt"

-- | The HTML <rtc> Element embraces semantic annotations of characters presented in a ruby of <rb> elements used inside of <ruby> element. <rb> elements can have both pronunciation (<rt>) and semantic (<rtc>) annotations.
rtc_ :: MonadBase Incremental m => m (ElementOptions "rtc")
  -> [Attribute "rtc"]
  -> m a
  -> m (Element, a)
rtc_ = element (Proxy :: Proxy "rtc") "rtc"

-- | The HTML <ruby> Element represents a ruby annotation. Ruby annotations are for showing pronunciation of East Asian characters.
ruby_ :: MonadBase Incremental m => m (ElementOptions "ruby")
  -> [Attribute "ruby"]
  -> m a
  -> m (Element, a)
ruby_ = element (Proxy :: Proxy "ruby") "ruby"

-- | The HTML Strikethrough Element (<s>) renders text with a strikethrough, or a line through it. Use the <s> element to represent things that are no longer relevant or no longer accurate. However, <s> is not appropriate when indicating document edits; for that, use the <del> and <ins> elements, as appropriate.
s_ :: MonadBase Incremental m => m (ElementOptions "s")
  -> [Attribute "s"]
  -> m a
  -> m (Element, a)
s_ = element (Proxy :: Proxy "s") "s"

-- | The HTML <samp> element is an element intended to identify sample output from a computer program. It is usually displayed in the browser's default monotype font (such as Lucida Console).
samp_ :: MonadBase Incremental m => m (ElementOptions "samp")
  -> [Attribute "samp"]
  -> m a
  -> m (Element, a)
samp_ = element (Proxy :: Proxy "samp") "samp"

-- | The HTML Small Element (<small>) makes the text font size one size smaller (for example, from large to medium, or from small to x-small) down to the browser's minimum font size.  In HTML5, this element is repurposed to represent side-comments and small print, including copyright and legal text, independent of its styled presentation.
small_ :: MonadBase Incremental m => m (ElementOptions "small")
  -> [Attribute "small"]
  -> m a
  -> m (Element, a)
small_ = element (Proxy :: Proxy "small") "small"

-- | The HTML <span> element is a generic inline container for phrasing content, which does not inherently represent anything. It can be used to group elements for styling purposes (using the class or id attributes), or because they share attribute values, such as lang. It should be used only when no other semantic element is appropriate. <span> is very much like a <div> element, but <div> is a block-level element whereas a <span> is an inline element.
span_ :: MonadBase Incremental m => m (ElementOptions "span")
  -> [Attribute "span"]
  -> m a
  -> m (Element, a)
span_ = element (Proxy :: Proxy "span") "span"

-- | The HTML Strong Element (<strong>) gives text strong importance, and is typically displayed in bold.
strong_ :: MonadBase Incremental m => m (ElementOptions "strong")
  -> [Attribute "strong"]
  -> m a
  -> m (Element, a)
strong_ = element (Proxy :: Proxy "strong") "strong"

-- | The HTML Subscript Element (<sub>) defines a span of text that should be displayed, for typographic reasons, lower, and often smaller, than the main span of text.
sub_ :: MonadBase Incremental m => m (ElementOptions "sub")
  -> [Attribute "sub"]
  -> m a
  -> m (Element, a)
sub_ = element (Proxy :: Proxy "sub") "sub"

-- | The HTML Superscript Element (<sup>) defines a span of text that should be displayed, for typographic reasons, higher, and often smaller, than the main span of text.
sup_ :: MonadBase Incremental m => m (ElementOptions "sup")
  -> [Attribute "sup"]
  -> m a
  -> m (Element, a)
sup_ = element (Proxy :: Proxy "sup") "sup"

-- | The HTML <time> element represents either a time on a 24-hour clock or a precise date in the Gregorian calendar (with optional time and timezone information).
time_ :: MonadBase Incremental m => m (ElementOptions "time")
  -> [Attribute "time"]
  -> m a
  -> m (Element, a)
time_ = element (Proxy :: Proxy "time") "time"

-- | The HTML Underline Element (<u>) renders text with an underline, a line under the baseline of its content.
u_ :: MonadBase Incremental m => m (ElementOptions "u")
  -> [Attribute "u"]
  -> m a
  -> m (Element, a)
u_ = element (Proxy :: Proxy "u") "u"

-- | The HTML Variable Element (<var>) represents a variable in a mathematical expression or a programming context.
var_ :: MonadBase Incremental m => m (ElementOptions "var")
  -> [Attribute "var"]
  -> m a
  -> m (Element, a)
var_ = element (Proxy :: Proxy "var") "var"

-- | The HTML element word break opportunity <wbr> represents a position within text where the browser may optionally break a line, though its line-breaking rules would not otherwise create a break at that location.
wbr_ :: MonadBase Incremental m => m (ElementOptions "wbr")
  -> [Attribute "wbr"]
  -> m Element
wbr_ = elementVoid (Proxy :: Proxy "wbr") "wbr"

-- | The HTML <area> element defines a hot-spot region on an image, and optionally associates it with a hypertext link. This element is used only within a <map> element.
area_ :: MonadBase Incremental m => m (ElementOptions "area")
  -> [Attribute "area"]
  -> m Element
area_ = elementVoid (Proxy :: Proxy "area") "area"

-- | The HTML <audio> element is used to embed sound content in documents. It may contain several audio sources, represented using the src attribute or the <source> element; the browser will choose the most suitable one.
audio_ :: MonadBase Incremental m => m (ElementOptions "audio")
  -> [Attribute "audio"]
  -> m a
  -> m (Element, a)
audio_ = element (Proxy :: Proxy "audio") "audio"

-- | The HTML <map> element is used with <area> elements to define an image map (a clickable link area).
map_ :: MonadBase Incremental m => m (ElementOptions "map")
  -> [Attribute "map"]
  -> m a
  -> m (Element, a)
map_ = element (Proxy :: Proxy "map") "map"

-- | The HTML <track> element is used as a child of the media elements—<audio> and <video>. It lets you specify timed text tracks (or time-based data), for example to automatically handle subtitles. The tracks are formatted in WebVTT format (.vtt files) — Web Video Text Tracks.
track_ :: MonadBase Incremental m => m (ElementOptions "track")
  -> [Attribute "track"]
  -> m Element
track_ = elementVoid (Proxy :: Proxy "track") "track"

-- | The HTML <video> element is used to embed video content. It may contain several video sources, represented using the src attribute or the <source> element; the browser will choose the most suitable one.
video_ :: MonadBase Incremental m => m (ElementOptions "video")
  -> [Attribute "video"]
  -> m a
  -> m (Element, a)
video_ = element (Proxy :: Proxy "video") "video"

-- | The HTML <embed> Element represents an integration point for an external application or interactive content (in other words, a plug-in).
embed_ :: MonadBase Incremental m => m (ElementOptions "embed")
  -> [Attribute "embed"]
  -> m Element
embed_ = elementVoid (Proxy :: Proxy "embed") "embed"

-- | The HTML Embedded Object Element (<object>) represents an external resource, which can be treated as an image, a nested browsing context, or a resource to be handled by a plugin.
object_ :: MonadBase Incremental m => m (ElementOptions "object")
  -> [Attribute "object"]
  -> m a
  -> m (Element, a)
object_ = element (Proxy :: Proxy "object") "object"

-- | The HTML <param> Element (or HTML Parameter Element) defines parameters for <object>.
param_ :: MonadBase Incremental m => m (ElementOptions "param")
  -> [Attribute "param"]
  -> m Element
param_ = elementVoid (Proxy :: Proxy "param") "param"

-- | The HTML <source> element is used to specify multiple media resources for <picture>, <audio> and <video> elements. It is an empty element. It is commonly used to serve the same media in multiple formats supported by different browsers.
source_ :: MonadBase Incremental m => m (ElementOptions "source")
  -> [Attribute "source"]
  -> m Element
source_ = elementVoid (Proxy :: Proxy "source") "source"

-- | The HTML <canvas> Element can be used to draw graphics via scripting (usually JavaScript). For example, it can be used to draw graphs, make photo compositions or even perform animations. You may (and should) provide alternate content inside the <canvas> block. That content will be rendered both on older browsers that don't support canvas and in browsers with JavaScript disabled.
canvas_ :: MonadBase Incremental m => m (ElementOptions "canvas")
  -> [Attribute "canvas"]
  -> m a
  -> m (Element, a)
canvas_ = element (Proxy :: Proxy "canvas") "canvas"

-- | The HTML <noscript> Element defines a section of html to be inserted if a script type on the page is unsupported or if scripting is currently turned off in the browser.
noscript_ :: MonadBase Incremental m => m (ElementOptions "noscript")
  -> [Attribute "noscript"]
  -> m a
  -> m (Element, a)
noscript_ = element (Proxy :: Proxy "noscript") "noscript"

-- | The HTML Script Element (<script>) is used to embed or reference an executable script within an HTML or XHTML document.
script_ :: MonadBase Incremental m => m (ElementOptions "script")
  -> [Attribute "script"]
  -> m a
  -> m (Element, a)
script_ = element (Proxy :: Proxy "script") "script"

-- | The HTML Deleted Text Element (<del>) represents a range of text that has been deleted from a document. This element is often (but need not be) rendered with strike-through text.
del_ :: MonadBase Incremental m => m (ElementOptions "del")
  -> [Attribute "del"]
  -> m a
  -> m (Element, a)
del_ = element (Proxy :: Proxy "del") "del"

-- | The HTML <ins> Element (or HTML Inserted Text) HTML represents a range of text that has been added to a document.
ins_ :: MonadBase Incremental m => m (ElementOptions "ins")
  -> [Attribute "ins"]
  -> m a
  -> m (Element, a)
ins_ = element (Proxy :: Proxy "ins") "ins"

-- | The HTML <caption> Element (or HTML Table Caption Element) represents the title of a table. Though it is always the first descendant of a <table>, its styling, using CSS, may place it elsewhere, relative to the table.
caption_ :: MonadBase Incremental m => m (ElementOptions "caption")
  -> [Attribute "caption"]
  -> m a
  -> m (Element, a)
caption_ = element (Proxy :: Proxy "caption") "caption"

-- | The HTML Table Column Element (<col>) defines a column within a table and is used for defining common semantics on all common cells. It is generally found within a <colgroup> element.
col_ :: MonadBase Incremental m => m (ElementOptions "col")
  -> [Attribute "col"]
  -> m Element
col_ = elementVoid (Proxy :: Proxy "col") "col"

-- | TODO
command_ :: MonadBase Incremental m => m (ElementOptions "command")
  -> [Attribute "command"]
  -> m Element
command_ = elementVoid (Proxy :: Proxy "command") "command"

-- | The HTML Table Column Group Element (<colgroup>) defines a group of columns within a table.
colgroup_ :: MonadBase Incremental m => m (ElementOptions "colgroup")
  -> [Attribute "colgroup"]
  -> m a
  -> m (Element, a)
colgroup_ = element (Proxy :: Proxy "colgroup") "colgroup"

-- | The HTML Table Element (<table>) represents data in two dimensions or more.
table_ :: MonadBase Incremental m => m (ElementOptions "table")
  -> [Attribute "table"]
  -> m a
  -> m (Element, a)
table_ = element (Proxy :: Proxy "table") "table"

-- | The HTML Table Body Element.
tbody_ :: MonadBase Incremental m => m (ElementOptions "tbody")
  -> [Attribute "tbody"]
  -> m a
  -> m (Element, a)
tbody_ = element (Proxy :: Proxy "tbody") "tbody"

-- | The Table cell HTML element (<td>) defines a cell of a table that contains data. It participates in the table model.
td_ :: MonadBase Incremental m => m (ElementOptions "td")
  -> [Attribute "td"]
  -> m a
  -> m (Element, a)
td_ = element (Proxy :: Proxy "td") "td"

-- | The HTML Table Foot Element (<tfoot>) defines a set of rows summarizing the columns of the table.
tfoot_ :: MonadBase Incremental m => m (ElementOptions "tfoot")
  -> [Attribute "tfoot"]
  -> m a
  -> m (Element, a)
tfoot_ = element (Proxy :: Proxy "tfoot") "tfoot"

-- | The HTML element table header cell <th> defines a cell that is a header for a group of cells of a table. The group of cells that the header refers to is defined by the scope and headers attribute.
th_ :: MonadBase Incremental m => m (ElementOptions "th")
  -> [Attribute "th"]
  -> m a
  -> m (Element, a)
th_ = element (Proxy :: Proxy "th") "th"

-- | The HTML Table Head Element (<thead>) defines a set of rows defining the head of the columns of the table.
thead_ :: MonadBase Incremental m => m (ElementOptions "thead")
  -> [Attribute "thead"]
  -> m a
  -> m (Element, a)
thead_ = element (Proxy :: Proxy "thead") "thead"

-- | The HTML element table row <tr> defines a row of cells in a table. Those can be a mix of <td> and <th> elements.
tr_ :: MonadBase Incremental m => m (ElementOptions "tr")
  -> [Attribute "tr"]
  -> m a
  -> m (Element, a)
tr_ = element (Proxy :: Proxy "tr") "tr"

-- | The HTML <button> Element represents a clickable button.
button_ :: MonadBase Incremental m => m (ElementOptions "button")
  -> [Attribute "button"]
  -> m a
  -> m (Element, a)
button_ = element (Proxy :: Proxy "button") "button"

-- | The HTML Datalist Element (<datalist>) contains a set of <option> elements that represent the values available for other controls.
datalist_ :: MonadBase Incremental m => m (ElementOptions "datalist")
  -> [Attribute "datalist"]
  -> m a
  -> m (Element, a)
datalist_ = element (Proxy :: Proxy "datalist") "datalist"

-- | The HTML <fieldset> element is used to group several controls as well as labels (<label>) within a web form.
fieldset_ :: MonadBase Incremental m => m (ElementOptions "fieldset")
  -> [Attribute "fieldset"]
  -> m a
  -> m (Element, a)
fieldset_ = element (Proxy :: Proxy "fieldset") "fieldset"

-- | The HTML <form> element represents a document section that contains interactive controls to submit information to a web server.
form_ :: MonadBase Incremental m => m (ElementOptions "form")
  -> [Attribute "form"]
  -> m a
  -> m (Element, a)
form_ = element (Proxy :: Proxy "form") "form"

-- | The HTML element <input> is used to create interactive controls for web-based forms in order to accept data from the user. How an <input> works varies considerably depending on the value of its type attribute.
input_ :: MonadBase Incremental m => m (ElementOptions "input")
  -> [Attribute "input"]
  -> m Element
input_ = elementVoid (Proxy :: Proxy "input") "input"

-- | The HTML Label Element (<label>) represents a caption for an item in a user interface. It can be associated with a control either by placing the control element inside the <label> element, or by using the @for@ attribute. Such a control is called the labeled control of the label element. One input can be associated with multiple labels.
label_ :: MonadBase Incremental m => m (ElementOptions "label")
  -> [Attribute "label"]
  -> m a
  -> m (Element, a)
label_ = element (Proxy :: Proxy "label") "label"

-- | The HTML <legend> Element (or HTML Legend Field Element) represents a caption for the content of its parent <fieldset>.
legend_ :: MonadBase Incremental m => m (ElementOptions "legend")
  -> [Attribute "legend"]
  -> m a
  -> m (Element, a)
legend_ = element (Proxy :: Proxy "legend") "legend"

-- | The HTML <meter> Element represents either a scalar value within a known range or a fractional value.
meter_ :: MonadBase Incremental m => m (ElementOptions "meter")
  -> [Attribute "meter"]
  -> m a
  -> m (Element, a)
meter_ = element (Proxy :: Proxy "meter") "meter"

-- | In a Web form, the HTML <optgroup> element  creates a grouping of options within a <select> element.
optgroup_ :: MonadBase Incremental m => m (ElementOptions "optgroup")
  -> [Attribute "optgroup"]
  -> m a
  -> m (Element, a)
optgroup_ = element (Proxy :: Proxy "optgroup") "optgroup"

-- | In a Web form, the HTML <option> element is used to create a control representing an item within a <select>, an <optgroup> or a <datalist> HTML5 element.
option_ :: MonadBase Incremental m => m (ElementOptions "option")
  -> [Attribute "option"]
  -> m a
  -> m (Element, a)
option_ = element (Proxy :: Proxy "option") "option"

-- | The HTML <output> element represents the result of a calculation or user action.
output_ :: MonadBase Incremental m => m (ElementOptions "output")
  -> [Attribute "output"]
  -> m a
  -> m (Element, a)
output_ = element (Proxy :: Proxy "output") "output"

-- | The HTML <progress> Element is used to view the completion progress of a task. While the specifics of how it's displayed is left up to the browser developer, it's typically displayed as a progress bar. Javascript can be used to manipulate the value of progress bar.
progress_ :: MonadBase Incremental m => m (ElementOptions "progress")
  -> [Attribute "progress"]
  -> m a
  -> m (Element, a)
progress_ = element (Proxy :: Proxy "progress") "progress"

-- | The HTML select (<select>) element represents a control that presents a menu of options. The options within the menu are represented by <option> elements, which can be grouped by <optgroup> elements. Options can be pre-selected for the user.
select_ :: MonadBase Incremental m => m (ElementOptions "select")
  -> [Attribute "select"]
  -> m a
  -> m (Element, a)
select_ = element (Proxy :: Proxy "select") "select"

-- | The HTML Details Element (<details>) is used as a disclosure widget from which the user can retrieve additional information.
details_ :: MonadBase Incremental m => m (ElementOptions "details")
  -> [Attribute "details"]
  -> m a
  -> m (Element, a)
details_ = element (Proxy :: Proxy "details") "details"

-- | The HTML <dialog> element represents a dialog box or other interactive component, such as an inspector or window. <form> elements can be integrated within a dialog by specifying them with the attribute method="dialog". When such a form is submitted, the dialog is closed with a returnValue attribute set to the value of the submit button used.
dialog_ :: MonadBase Incremental m => m (ElementOptions "dialog")
  -> [Attribute "dialog"]
  -> m a
  -> m (Element, a)
dialog_ = element (Proxy :: Proxy "dialog") "dialog"

-- | The HTML <menu> element represents a group of commands that a user can perform or activate. This includes both list menus, which might appear across the top of a screen, as well as context menus, such as those that might appear underneath a button after it has been clicked.
menu_ :: MonadBase Incremental m => m (ElementOptions "menu")
  -> [Attribute "menu"]
  -> m a
  -> m (Element, a)
menu_ = element (Proxy :: Proxy "menu") "menu"

-- | The HTML <menuitem> element represents a command that a user is able to invoke through a popup menu. This includes context menus, as well as menus that might be attached to a menu button.
menuitem_ :: MonadBase Incremental m => m (ElementOptions "menuitem")
  -> [Attribute "menuitem"]
  -> m a
  -> m (Element, a)
menuitem_ = element (Proxy :: Proxy "menuitem") "menuitem"

-- | The HTML summary element (<summary>) is used as a summary, caption, or legend for the content of a <details> element.
summary_ :: MonadBase Incremental m => m (ElementOptions "summary")
  -> [Attribute "summary"]
  -> m a
  -> m (Element, a)
summary_ = element (Proxy :: Proxy "summary") "summary"

-- | The HTML <content> element is used inside of Shadow DOM as an insertion point. It is not intended to be used in ordinary HTML. It is used with Web Components.
content_ :: MonadBase Incremental m => m (ElementOptions "content")
  -> [Attribute "content"]
  -> m a
  -> m (Element, a)
content_ = element (Proxy :: Proxy "content") "content"

-- | TODO
decorator_ :: MonadBase Incremental m => m (ElementOptions "decorator")
  -> [Attribute "decorator"]
  -> m a
  -> m (Element, a)
decorator_ = element (Proxy :: Proxy "decorator") "decorator"

-- | The HTML <element> element is used to define new custom DOM elements.
element_ :: MonadBase Incremental m => m (ElementOptions "element")
  -> [Attribute "element"]
  -> m a
  -> m (Element, a)
element_ = element (Proxy :: Proxy "element") "element"

-- | The HTML <shadow> element is used as a shadow DOM insertion point. You might use it if you have created multiple shadow roots under a shadow host. It is not useful in ordinary HTML. It is used with Web Components.
shadow_ :: MonadBase Incremental m => m (ElementOptions "shadow")
  -> [Attribute "shadow"]
  -> m a
  -> m (Element, a)
shadow_ = element (Proxy :: Proxy "shadow") "shadow"

-- | The HTML template element <template> is a mechanism for holding client-side content that is not to be rendered when a page is loaded but may subsequently be instantiated during runtime using JavaScript.
template_ :: MonadBase Incremental m => m (ElementOptions "template")
  -> [Attribute "template"]
  -> m a
  -> m (Element, a)
template_ = element (Proxy :: Proxy "template") "template"

-- | The HTML <link> element specifies relationships between the current document and an external resource. Possible uses for this element include defining a relational framework for navigation. This Element is most used to link to style sheets.
link_ :: MonadBase Incremental m => m (ElementOptions "link")
  -> [Attribute "link"]
  -> m Element
link_ = elementVoid (Proxy :: Proxy "link") "link"

-- | The HTML <meta> element represents any metadata information that cannot be represented by one of the other HTML meta-related elements (<base>, <link>, <script>, <style> or <title>).
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
