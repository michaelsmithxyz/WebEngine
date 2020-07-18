module WebEngine.Parse.Css

open FParsec
open WebEngine.Data.Css


let pTypeSelector: Parser<TypeSelector, unit> =
    let pUniversal = pstring "*" |>> fun _ -> Universal
    let pTagName = regex "[A-z]+" |>> Element
    choice [
        pUniversal
        pTagName
    ]

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