module WebEngine.Data.Css


(* Selectors *)

/// A type representing a CSS "type" selector, which matches either all
/// elements with the given element name, or the universal selector '*',
/// which matches all elements
type TypeSelector =
    | Universal
    | Element of string
    
/// A type representing a CSS "subclass" selector, which matches elements
/// with the given class or id attributes
type SubclassSelector =
    | Class of string
    | Id of string

/// A type representing a CSS "compound" selector, which matches all
/// elements that match the given type selector, if provided, and all given
/// subclass selectors. It's required that either a type selector or at least
/// one subclass selector is provided (i.e. this cannot be empty)
type CompoundSelector = {
    TypeSelector: TypeSelector option
    SubclassSelectors: SubclassSelector list
}

/// A type representing a CSS selector combinator, which defines relationships
/// between selectors that it operates on
type Combinator =
    | Descendant
    | Child

/// A type representing a CSS "complex" selector, which consists of one or
/// more compound selectors joined by combinators. This type is defined
/// recursively in a similar way to a linked list, with each link
/// associated with a combinator
type ComplexSelector =
    | Compound of CompoundSelector
    | Combined of CompoundSelector * Combinator * ComplexSelector

/// A type representing a CSS selector list, which consists of one or more
/// complex selectors
type SelectorList = ComplexSelector list


(* Stylesheets *)

/// A type representing a CSS property (color, font-family, etc.)
type Property = string

/// A type representing a CSS property value
type Value = string

/// A type representing a CSS property declaration
type Declaration = {
    Property: Property
    Value: Value
}

/// A type representing a CSS style rule, which is a rule which applies property
/// values to selectors. This is essentially a CSS "block".
type StyleRule = {
    Selector: SelectorList
    Declarations: Declaration list
}

/// A type representing a complete CSS stylesheet
type Stylesheet = {
    Rules: StyleRule list
}
