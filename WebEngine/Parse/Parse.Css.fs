module WebEngine.Parse.Css

open FParsec
open WebEngine.Data.Css


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
    