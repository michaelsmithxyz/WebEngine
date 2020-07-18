/// An FParsec-based parser implementation for a very limited subset of HTML
module WebEngine.Parse.Html

open FParsec
open WebEngine.Data.Html


/// Parse a single HTML attribute, including any trailing spaces
let pAttribute: Parser<Attribute, unit> =
    let pAttributeName = regex "[A-z]+"
    let pAttributeBody = parse {do! skipChar '"'
                                let! body = regex "[A-z\\s\\d]*"
                                do! skipChar '"'
                                return body}
    parse {let! attributeName = pAttributeName
           do! spaces
           do! skipString "="
           do! spaces
           let! attributeBody = pAttributeBody
           do! spaces
           return (attributeName, attributeBody)}
    |>> Attribute
    
/// Parse a sequence of zero or more HTML attributes
let pAttributes: Parser<Attribute list, unit> = many pAttribute


let private pElementImpl, private pElementRef = createParserForwardedToRef<Element, unit>()
let private pElementsImpl, private pElementsRef = createParserForwardedToRef<Element list, unit>()
    

/// Parse an opening tag, including any attributes, with the given tag name.
/// The resulting tag will be constructed with the given tag type.
let pTagOfType (tagType: TagType) (name: string): Parser<Tag, unit> =
    let pTagOpen = parse {do! skipChar '<'
                          do! spaces
                          do! skipStringCI name
                          do! spaces}
    parse {do! attempt pTagOpen
           let! attributes = pAttributes
           do! skipString ">"
           do! spaces
           return attributes}
    |>> fun attributes ->
        { Type = tagType
          Attributes = attributes }

/// Parse a full tag element, including children, with the given tag name,
/// returning a tag element of the given tag type. This parser also consumes
/// any whitespace following the closing tag.
let pTagElementOfType (tagType: TagType) (name: string): Parser<Element, unit> =
    let pClosingTag = parse {do! skipString "</"
                             do! spaces
                             do! skipStringCI name
                             do! spaces
                             do! skipString ">"
                             do! spaces}
    parse {let! tag = pTagOfType tagType name
           let! children = pElementsImpl
           do! pClosingTag
           return (tag, children)}
    |>> fun (tag, children) -> TagElement(tag, children)
    
/// Parse a single `html` tag element
let pHtmlTagElement: Parser<Element, unit> = pTagElementOfType Html "html"
/// Parse a single `body` tag element
let pBodyTagElement: Parser<Element, unit> = pTagElementOfType Body "body"
/// Parse a single `div` tag element
let pDivTagElement: Parser<Element, unit> = pTagElementOfType Div "div"
/// Parse a single `span` tag element
let pSpanTagElement: Parser<Element, unit> = pTagElementOfType Span "span"
 
/// Parse a single tag element, of any supported tag type       
let pTagElement: Parser<Element, unit> =
    choice [
        pHtmlTagElement
        pBodyTagElement
        pDivTagElement
        pSpanTagElement
    ]
    
/// Parse a text element, which is any sequence of character up to the next
/// angle bracket
let pTextElement: Parser<Element, unit> =
    many1Satisfy (isNoneOf "<>") |>> fun s -> TextElement(s.Trim())

do pElementsRef :=
    many pElementImpl

do pElementRef :=
    choice [
        pTextElement
        pTagElement
    ]
    
/// Parse a single element, of any type
let pElement: Parser<Element, unit> = pElementImpl

/// Parse a sequence of zero or more elements, of any type
let pElements: Parser<Element list, unit> = pElementsImpl

/// Parse an HTML document, which consists of a single root
/// tag of type `html`, optionally surrounded by whitespace
let pDocument: Parser<Document, unit> =
    parse {do! spaces
           let! root = pHtmlTagElement
           return root}
    |>> fun root -> { Root = root }