module WebEngine.Tests.Parse.Css.Tests

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
    
    
let testParseCompoundSelectorCases =
    [
        "html", { TypeSelector = Some(Element("html")); SubclassSelectors = [] }
        "#id", { TypeSelector = None; SubclassSelectors = [ Id("id") ] }
        ".class", { TypeSelector = None; SubclassSelectors = [ Class("class") ] }
        
        ("div.class",
         { TypeSelector = Some(Element("div"))
           SubclassSelectors = [ Class("class") ] })
        ("div#id",
         { TypeSelector = Some(Element("div"))
           SubclassSelectors = [ Id("id") ] })
        ("div.class-one.class-two",
         { TypeSelector = Some(Element("div"))
           SubclassSelectors = [ Class("class-one"); Class("class-two") ] })
        ("div.class#id",
         { TypeSelector = Some(Element("div"))
           SubclassSelectors = [ Class("class"); Id("id") ] })
        
        (".class#id",
         { TypeSelector = None
           SubclassSelectors = [ Class("class"); Id("id") ] })
        (".class-one.class-two",
         { TypeSelector = None
           SubclassSelectors = [ Class("class-one"); Class("class-two") ] })
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseCompoundSelectorCases")>]
let ``test that we can parse compound selectors`` (input: string) (expected: CompoundSelector) =
    let parsed = run pCompoundSelector input
    parsed |> should be (parsedAs expected)