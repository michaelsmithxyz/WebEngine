module WebEngine.Data.Css


type TypeSelector =
    | Universal
    | Element of string
    
type SubclassSelector =
    | Class of string
    | Id of string
    // TODO - Add attribute

type CompoundSelector = {
    TypeSelector: TypeSelector option
    SubclassSelectors: SubclassSelector list
}

type Combinator =
    | Descendant

type ComplexSelector =
    | Compound of CompoundSelector
    | Combined of CompoundSelector * Combinator * ComplexSelector

type SelectorList = ComplexSelector list