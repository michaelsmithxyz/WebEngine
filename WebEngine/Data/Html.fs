module WebEngine.Data.Html


/// A type representing valid HTML tags
type TagType =
    | Html
    | Body
    | Div
    | Span
    
// A type representing a single HTML attribute, with a name and value
type Attribute = Attribute of string * string
    
// A type representing a single HTML tag
type Tag =
    {
        Type: TagType
        Attributes: Attribute list
    }

/// A type representing an HTML element
type Element =
    // A tag element is a normal HTML tag with tag data and children
    | TagElement of Tag * Element list
    // A text element is free text which can appear within tags
    | TextElement of string
    
/// A type representing an HTML document
type Document = { Root: Element }


/// Convert a list of (string * string) tuples to a list of Attributes
let toAttributes (attributes: (string * string) list): Attribute list =
    attributes |> List.map Attribute
    
// Construct a tag element with the given tag type, attributes, and children
let buildTagElement
    (tagType: TagType)
    (attributes : (string * string) list)
    (children: Element list): Element =
    TagElement({ Type = tagType; Attributes = toAttributes attributes }, children)


// Construct an `html` element with the given attributes and children
let HtmlTag = buildTagElement Html
// Construct a `body` element with the given attributes and children
let BodyTag  = buildTagElement Body
// Construct a `div` element with the given attributes and children
let DivTag = buildTagElement Div
// Construct a `span` element with the given attributes and children
let SpanTag = buildTagElement Span

// Construct a text element with the given text
let Text = TextElement