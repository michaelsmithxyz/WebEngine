module WebEngine.Tests.Utilities.Constraints

open FParsec.CharParsers
open NUnit.Framework.Constraints

/// A custom NUnit constraint which compares an FParsec ParserResult
/// with an expected value. If the ParserResult is a Failure, or the
/// result is a Success, but the resulting value does not match the
/// expected value, the constraint fails.
type ParsesAsConstraint<'TExpected when 'TExpected : equality>(expected: 'TExpected) =
    inherit Constraint(expected)
    member this.Expected = expected
    
    override this.ApplyTo<'TActual>(actual: 'TActual) =
        match box actual with
        | :? ParserResult<'TExpected, unit> as parserResult ->
            match parserResult with
            | Success (r, _, _) -> ConstraintResult(this, actual, this.Expected.Equals(r))
            | Failure (msg, _, _) -> ConstraintResult(this, msg, false)
        | _ -> ConstraintResult(this, actual, false)
            
/// Fluently construct a ParsesAsConstraint instance. This is useful for FUnit
let parsedAs expected = ParsesAsConstraint(expected)