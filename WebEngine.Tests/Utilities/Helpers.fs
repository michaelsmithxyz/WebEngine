module WebEngine.Tests.Utilities.Helpers

open NUnit.Framework


// The following helpers convert lists of tuple (of various sizes) to
// lists of TestCaseData instances for use in NUnit tests

let toTestCases2 (cases: ('a * 'b) list): TestCaseData list =
    List.map (fun (a, b) -> TestCaseData(a, b)) cases
    
let toTestCases3 (cases: ('a * 'b * 'c) list): TestCaseData list =
    List.map (fun (a, b, c) -> TestCaseData(a, b, c)) cases
    
let toTestCases4 (cases: ('a * 'b * 'c * 'd) list): TestCaseData list =
    List.map (fun (a, b, c, d) -> TestCaseData(a, b, c, d)) cases