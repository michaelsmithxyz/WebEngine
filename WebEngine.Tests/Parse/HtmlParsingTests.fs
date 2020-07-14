module WebEngine.Tests.Parse.HtmlParsingTests

open NUnit.Framework
open FsUnit
open FParsec
open WebEngine.Data.Html
open WebEngine.Parse.Html
open WebEngine.Tests.Utilities.Helpers
open WebEngine.Tests.Utilities.Constraints

    
let private testParseAttributeCases =
    [
        "id=\"test\"", Attribute("id", "test")
        "class=\"class1 class2\"", Attribute("class", "class1 class2")
        "width=\"100px\"", Attribute("width", "100px")
        
        "id = \"TEST\"", Attribute("id", "TEST")
        "id = \"TEST2\"  ", Attribute("id", "TEST2")
    ] |> toTestCases2

[<Test; TestCaseSource("testParseAttributeCases")>]
let ``test that we can parse individual attributes``(input: string, expected: Attribute) =
    let parsed = run pAttribute input
    parsed |> should be (parsedAs expected)
     
        
let private testParseAttributesCases =
    [
        "", []
        "class=\"class1 class2\"", [Attribute("class", "class1 class2")]
        
        ("id=\"element\" class=\"class1\"",
         [Attribute("id", "element"); Attribute("class", "class1")])
        
    ] |> toTestCases2

[<Test; TestCaseSource("testParseAttributesCases")>]
let ``test that we can parse collections of attributes``(input: string, expected: Attribute list) =
    let parsed = run pAttributes input
    parsed |> should be (parsedAs expected)


let private testParseTagCases =
    [
        "<html>", Html, "html", { Type = Html; Attributes = []  }
        "<body>", Body, "body", { Type = Body; Attributes = []  }
        "<div>", Div, "div", { Type = Div; Attributes = []  }
        "<span>", Span, "span", { Type = Span; Attributes = []  }
        
        "< html >", Html, "html", { Type = Html; Attributes = []  }
        "<HTML>", Html, "html", { Type = Html; Attributes = []  }
        
        ("<div id=\"content\">", Div, "div",
         { Type = Div; Attributes = [Attribute("id", "content")] })
        
        ("<span id=\"content\" class=\"wrapper\">", Span, "span",
         { Type = Span
           Attributes = [
               Attribute("id", "content"); Attribute("class", "wrapper") ]})
    ] |> toTestCases4
    
[<Test; TestCaseSource("testParseTagCases")>]
let ``test that we can parse individual tags``(input: string, tagType: TagType, name: string, expected: Tag) =
    let parsed = run (pTagOfType tagType name) input
    parsed |> should be (parsedAs expected)
        

let private testParseTagElementCases =
    [
        "<div></div>", DivTag [] []
        
        ("<span id=\"a\"></span>",
         SpanTag [("id", "a")] [])
        
        ("<div><span></span></div>",
         DivTag [] [
             SpanTag [] []
         ])
        
        ("<div><span id=\"one\"></span><span id=\"two\"></span></div>",
         DivTag [] [
             SpanTag [("id", "one")] []
             SpanTag [("id", "two")] []
         ])
        
    ] |> toTestCases2

[<Test; TestCaseSource("testParseTagElementCases")>]
let ``test that we can parse individual tag elements`` (input: string, expected: Element) =
    let parsed = run pTagElement input
    parsed |> should be (parsedAs expected)

    
let testParseTextCases =
    [
        "abc", Text "abc"
        "123", Text "123"
        "abc </span>", Text "abc"
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseTextCases")>]
let ``test that we can parse individual text elements`` (input: string, expected: Element) =
    let parsed = run pTextElement input
    parsed |> should be (parsedAs expected)
    

let testParseElementCases =
    [
        "abc", Text "abc"
        "<span></span>", SpanTag [] []
        
        ("<span>abc</span>",
         SpanTag [] [Text "abc"])
        
        ("<span>a<span>b</span>c</span>",
         SpanTag [] [
             Text "a"
             SpanTag [] [
                 Text "b"
             ]
             Text "c"
         ])
        
        ("<html>a<span id=\"middle\">b</span>c</html>",
         HtmlTag [] [
             Text "a"
             SpanTag [("id", "middle")] [
                 Text "b"
             ]
             Text "c"
         ])
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseElementCases")>]
let ``test that we can parse individual elements`` (input: string, expected: Element) =
    let parsed = run pElement input
    parsed |> should be (parsedAs expected)
    
    
let testParseDocumentCases =
    [
        ("<html><div>One</div><div>Two</div><div>Three</div></html>",
         {
             Root = HtmlTag [] [
                 DivTag [] [Text "One"]
                 DivTag [] [Text "Two"]
                 DivTag [] [Text "Three"]
             ]
         })
    ] |> toTestCases2
    
[<Test; TestCaseSource("testParseDocumentCases")>]
let ``test that we can parse documents`` (input: string, expected: Document) =
    let parsed = run pDocument input
    parsed |> should be (parsedAs expected)