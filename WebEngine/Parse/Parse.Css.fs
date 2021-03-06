module WebEngine.Parse.Css

open FParsec
open WebEngine.Data.Css


let private requireSpaces = skipMany1 (pchar ' ')
let private skipSpaces = skipMany (pchar ' ')


(* Selector parsing *)

/// Parse a single CSS type selector, including the universal selector
let pTypeSelector: Parser<TypeSelector, unit> =
    let pUniversal = pstring "*" |>> fun _ -> Universal
    let pTagName = regex "[A-z]+" |>> Element
    choice [
        pUniversal
        pTagName
    ]

/// Parse a single CSS subclass selector
let pSubclassSelector: Parser<SubclassSelector, unit> =
    let pIdSelector = parse {do! skipChar '#'
                             let! id = regex "[A-z0-9_\\-]+"
                             return id} |>> Id
    let pClassSelector = parse {do! skipChar '.'
                                let! cls = regex "[A-z0-9_\\-]+"
                                return cls} |>> Class
    choice [
        pIdSelector
        pClassSelector
    ]

/// Parse a single CSS compound selector, consisting of an optional type
/// selector, followed by zero or more subclass selectors, with the restriction
/// that this parser cannot produce an "empty" selector, without a type and with
/// zero subclass selectors
let pCompoundSelector: Parser<CompoundSelector, unit> =
    let pMaybeEmptyCompoundSelector = parse {let! typeSelector = opt pTypeSelector
                                             let! subclassSelectors = many pSubclassSelector
                                             return typeSelector, subclassSelectors}
    notEmpty pMaybeEmptyCompoundSelector
    |>> fun (t, s) ->
            { TypeSelector = t
              SubclassSelectors = s }


/// Parse a single CSS combinator
let pCombinator: Parser<Combinator, unit> =
    let pDescendent = parse {do! requireSpaces} |>> fun _ -> Descendant
    let pChild = parse {do! skipSpaces
                        do! skipChar '>'
                        do! skipSpaces} |>> fun _ -> Child
    choice [
        attempt pChild
        attempt pDescendent
    ]


let private pComplexSelectorImpl, private pComplexSelectorRef =
    createParserForwardedToRef<ComplexSelector, unit>()

do pComplexSelectorRef :=
    (* pTail is a tiny bit subtle due to the fact that the space character is
       overloaded as both a combinator and a token separator in a contextual way.
       A space can only be considered a combinator in this context if it's followed
       by a valid compound selector. Otherwise, there's some chance that it may
       be followed by a token which should be handled further up the parser tree
       (like a comma separating selectors in a list), so we need to bail out in
       order to let those tokens be handled appropriately. *)
    let pTail = parse {let! combinator = pCombinator
                       do! followedBy pCompoundSelector
                       let! rest = pComplexSelectorImpl
                       return (combinator, rest)}
    let pCombined = parse {let! compound = pCompoundSelector
                           let! (combinator, rest) = pTail
                           return (compound, combinator, rest)} |>> Combined
    choice [
        attempt pCombined
        pCompoundSelector |>> Compound
    ]

/// Parse a complex selector, which consists of a compound selector, combined
/// optionally via a combinator with another complex selector. This
/// combinator-separated recursive definition allows for chains of combinators
let pComplexSelector: Parser<ComplexSelector, unit> = pComplexSelectorImpl


/// Parse a selector list, which is a comma-delimited list of complex selectors
let pSelectorList: Parser<SelectorList, unit> =
    let pDelim = parse {do! spaces
                        do! skipChar ','
                        do! spaces}
    sepBy1 pComplexSelector (attempt pDelim) .>> spaces


(* Stylesheet parsing *)

/// Parse a single CSS property
let pProperty: Parser<Property, unit> = regex "[\-A-z]+"

/// Parse a single CSS property value
let pValue: Parser<Value, unit> = regex "[0-9A-z#\\s\\(\\),\\.]+"

/// Parse a single CSS declaration
let pDeclaration: Parser<Declaration, unit> =
    parse {do! skipSpaces
           let! property = pProperty
           do! skipChar ':'
           do! skipSpaces
           let! value = pValue
           do! skipSpaces
           do! skipChar ';'
           do! spaces
           return (property, value)} |>>
    fun (p, v) ->
        { Property = p
          Value = v }

/// Parse a single style rule, or CSS block
let pStyleRule: Parser<StyleRule, unit> =
    let pRuleOpening = parse {do! spaces
                              let! selectors = pSelectorList
                              do! skipChar '{'
                              do! spaces
                              return selectors}
    let pDeclarations = many pDeclaration
    let pRuleClose = parse {do! spaces
                            do! skipChar '}'
                            do! spaces}
    parse {let! selectors = pRuleOpening
           let! decls = pDeclarations
           do! pRuleClose
           return (selectors, decls)} |>>
    fun (s, d) ->
        { Selector = s
          Declarations = d }
