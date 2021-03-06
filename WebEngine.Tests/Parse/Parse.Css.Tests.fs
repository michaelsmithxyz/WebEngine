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
        
        ("div ,",
         { TypeSelector = Some(Element("div"))
           SubclassSelectors = [] })
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseCompoundSelectorCases")>]
let ``test that we can parse compound selectors`` (input: string) (expected: CompoundSelector) =
    let parsed = run pCompoundSelector input
    parsed |> should be (parsedAs expected)


let private testParseCombinatorCases =
    [
        " ", Descendant
        "  ", Descendant
        
        ">", Child
        " >", Child
        "> ", Child
        " > ", Child
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseCombinatorCases")>]
let ``test that we can parse combinators`` (input: string) (expected: Combinator) =
    let parsed = run pCombinator input
    parsed |> should be (parsedAs expected)


let private testParseComplexCases =
    [
        ("div.class",
         Compound({ TypeSelector = Some(Element("div"))
                    SubclassSelectors = [ Class("class") ] }))
        
        ("div.class span#id",
         Combined({ TypeSelector = Some(Element("div"))
                    SubclassSelectors = [ Class("class") ] },
                   Descendant,
                   Compound({ TypeSelector = Some(Element("span"))
                              SubclassSelectors = [ Id("id") ] })))
        
        ("html > body",
         Combined({ TypeSelector = Some(Element("html"))
                    SubclassSelectors = [] },
                   Child,
                   Compound({ TypeSelector = Some(Element("body"))
                              SubclassSelectors = [] })))
        ("html>body",
         Combined({ TypeSelector = Some(Element("html"))
                    SubclassSelectors = [] },
                   Child,
                   Compound({ TypeSelector = Some(Element("body"))
                              SubclassSelectors = [] })))
        
        ("div ,",
         Compound({ TypeSelector = Some(Element("div"))
                    SubclassSelectors = [] }))
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseComplexCases")>]
let ``test that we can parse complex selectors`` (input: string) (expected: ComplexSelector) =
    let parsed = run pComplexSelector input
    parsed |> should be (parsedAs expected)


let private testParseSelectorListCases =
    [
        ("div",
         [
             Compound({ TypeSelector = Some(Element("div"))
                        SubclassSelectors = [] })
         ])
        ("div ",
         [
             Compound({ TypeSelector = Some(Element("div"))
                        SubclassSelectors = [] })
         ])
        ("div, span",
         [
             Compound({ TypeSelector = Some(Element("div"))
                        SubclassSelectors = [] })
             Compound({ TypeSelector = Some(Element("span"))
                        SubclassSelectors = [] })
         ])
        ("div , span",
         [
             Compound({ TypeSelector = Some(Element("div"))
                        SubclassSelectors = [] })
             Compound({ TypeSelector = Some(Element("span"))
                        SubclassSelectors = [] })
         ])
        ("div ,span",
         [
             Compound({ TypeSelector = Some(Element("div"))
                        SubclassSelectors = [] })
             Compound({ TypeSelector = Some(Element("span"))
                        SubclassSelectors = [] })
         ])
        ("div, span > a",
         [
             Compound({ TypeSelector = Some(Element("div"))
                        SubclassSelectors = [] })
             Combined({ TypeSelector = Some(Element("span"))
                        SubclassSelectors = [] },
                     Child,
                     Compound({ TypeSelector = Some(Element("a"))
                                SubclassSelectors = [] }))
         ])
        ("html > body, span > a",
         [
             Combined({ TypeSelector = Some(Element("html"))
                        SubclassSelectors = [] },
                     Child,
                     Compound({ TypeSelector = Some(Element("body"))
                                SubclassSelectors = [] }))
             Combined({ TypeSelector = Some(Element("span"))
                        SubclassSelectors = [] },
                     Child,
                     Compound({ TypeSelector = Some(Element("a"))
                                SubclassSelectors = [] }))
         ])
        ("html > body.container, span > a#id",
         [
             Combined({ TypeSelector = Some(Element("html"))
                        SubclassSelectors = [] },
                     Child,
                     Compound({ TypeSelector = Some(Element("body"))
                                SubclassSelectors = [ Class("container") ] }))
             Combined({ TypeSelector = Some(Element("span"))
                        SubclassSelectors = [] },
                     Child,
                     Compound({ TypeSelector = Some(Element("a"))
                                SubclassSelectors = [ Id("id") ] }))
         ])
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseSelectorListCases")>]
let ``test that we can parse selector lists`` (input: string) (expected: SelectorList) =
    let parsed = run pSelectorList input
    parsed |> should be (parsedAs expected)


let testParsePropertyCases =
    [
        ("color", "color")
        ("color:", "color")
        ("background-color", "background-color")
        ("background-color:", "background-color")
        ("-webkit-experimental-property", "-webkit-experimental-property")
    ] |> toTestCases2

[<Test; TestCaseSource("testParsePropertyCases")>]
let ``test that we can parse properties`` (input: string) (expected: Property) =
    let parsed = run pProperty input
    parsed |> should be (parsedAs expected)


let testParseValueCases =
    [
        ("red", "red")
        ("blue;", "blue")
        ("10px 10px", "10px 10px")
        ("1px solid red;", "1px solid red")
        ("#FFFFFF", "#FFFFFF")
        ("rgba(1.0, 1.0, 1.0, 1.0);", "rgba(1.0, 1.0, 1.0, 1.0)")
    ] |> toTestCases2

[<Test; TestCaseSource("testParseValueCases")>]
let ``test that we can parse values`` (input: string) (expected: Value) =
    let parsed = run pValue input
    parsed |> should be (parsedAs expected)


let testParseDeclarationCases =
    [
        ("color: red;", { Property = "color"; Value = "red" })
        ("background-color:   #fff;   ", { Property = "background-color"; Value = "#fff" })
        ("-webkit-border-radius: 12px;", { Property = "-webkit-border-radius"; Value = "12px" })
    ] |> toTestCases2

[<Test; TestCaseSource("testParseDeclarationCases")>]
let ``test that we can parse declarations`` (input: string) (expected: Declaration) =
    let parsed = run pDeclaration input
    parsed |> should be (parsedAs expected)


let testParseStyleRuleCases =
    [
        ("p {
             color: red;
         }",
         {
             Selector = [Compound({ TypeSelector = Some(Element("p"))
                                    SubclassSelectors = [] })]
             Declarations = [{ Property = "color"; Value = "red" }]
         })
        (* TODO - more test cases here *)
    ] |> toTestCases2

[<Test; TestCaseSource("testParseStyleRuleCases")>]
let ``test that we can parse style rules`` (input: string) (expected: StyleRule) =
    let parsed = run pStyleRule input
    parsed |> should be (parsedAs expected)