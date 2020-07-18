module WebEngine.Tests.Parse.CssParsingTests

open FParsec
open NUnit.Framework
open FsUnit
open WebEngine.Data.Css
open WebEngine.Parse.Css
open WebEngine.Tests.Utilities.Helpers
open WebEngine.Tests.Utilities.Constraints


let private testParseTypeSelectorCases =
    [
        "*", Universal
        "* ", Universal
        
        "html", Element("html")
        "html ", Element("html")
        "div", Element("div")
        "div ", Element("div")
    ] |> toTestCases2

[<Test; TestCaseSource("testParseTypeSelectorCases")>]
let ``test that we can parse type selectors`` (input: string) (expected: TypeSelector) =
    let parsed = run pTypeSelector input
    parsed |> should be (parsedAs expected)
    
    
let private testParseSubclassSelectorCases =
    [
        "#id", Id("id")
        "#id ", Id("id")
        "#multi_part_id", Id("multi_part_id")
        "#multi-part-id", Id("multi-part-id")
        
        ".class", Class("class")
        ".class ", Class("class")
        ".multi_part_class", Class("multi_part_class")
        ".multi-part-class", Class("multi-part-class")
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseSubclassSelectorCases")>]
let ``test that we can parse subclass selectors`` (input: string) (expected: SubclassSelector) =
    let parsed = run pSubclassSelector input
    parsed |> should be (parsedAs expected)