# TypeScript Types Script

A mini-language that is functional, pure, and (ironically) untyped. Its goal is to compute arbitrary complex things using only TypeScript's type system.

# Syntax

## Atom

Atom is just a symbol that is used as a concrete value.

## Functions

Functions accept a fixed number of arguments and return one value. Functions can be recursive. Functions support top-level pattern-matching.

Functions can be marked as "passable" with a syntax (get converted to a string in a globally assigned array).

## Data Structures

Lists - lists contain atoms.

Dictionaries - dictionaries define surjective mappings of atoms to atoms.

Numbers - actually just a syntactic sugar on top of lists using atoms 0-9.

Strings - actually just a syntactic sugar on top of lists using atoms 0-225 (ascii).

Set - a set of atoms, can't be used too large (limitation of TS type system).

## Examples

```
    f a b = (add a b)

    symbol = #true
    
    list = [#0, #1, #abracadabra]
    
    dictionary = { #key: #value, #another: list }
    
    set = { #one, #two, #three }
    
    string = "this is a string"
    
    number = 123445

    fib n =
    | (gt n 2) -> 1
    | otherwise -> (add (fib n-1) (fib n-2))
```


# Prior Art

A scripting language implemented as a DSL of the generic type system. It features: parser and a AST-evaluator:

[https://github.com/fightingcat/sits](https://github.com/fightingcat/sits)

A 4-bit stack VM that executes a custom instruction set, an assembly-program is inputed as a generic type parameter: [https://gist.github.com/acutmore/9d2ce837f019608f26ff54e0b1c23d6e](https://gist.github.com/acutmore/9d2ce837f019608f26ff54e0b1c23d6e)

[https://github.com/Microsoft/TypeScript/issues/14833](https://github.com/Microsoft/TypeScript/issues/14833)
