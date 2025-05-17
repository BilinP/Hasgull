{-|
Module      : FormalSyntax
Description : Formal syntax definition for the Hasgull programming language.

This module provides a formal syntax definition for Hasgull in an extended
EBNF-style notation. Copy and paste the following definitions as a reference.

== Formal Syntax Definition

=== Types and Identifiers

* __var__ Represents a variable.

* __structname__ Represents the name of a struct.

* __traitname__  Represents the name of a trait (typeclass).

* __i__  Represents an integer.

=== Concrete Syntax
@
__comma_type__ ::= [type (`,` type)*]
@


@
__type__ ::=
         `Int` | `Void` | `Boolean` | /Built-in types/
         `Self` | /Refers to our own type in a trait/
         structname | /Structs are a valid kind of type/
         `(` type `)` | /Parenthesized type/
         `(` comma_type `)` `=>` type /Higher-order function/
@


@
__param__ ::= var `:` type
@


@
__comma_param__ ::= [param (`,` param)*]
@


@
__structdef__ ::= `struct` structname `{` comma_param `}`
@


@
/Definition of an abstract method/
__abs_methoddef__ ::= `method` var (` comma_param `)` `:` type `;`
@


@
/Definition of a concrete method/
__conc_methoddef__ ::= `method` var (` comma_param `)` `:` type `{` stmt* `}`
@


@
/Definition of a trait (typeclass)/
__traitdef__ ::= `trait` traitname `{` abs_methoddef* `}`
@


@
/Definition of an implementation of a typeclass/
__impldef__ ::= `impl` traitname `for` type `{` conc_methoddef* `}`
@


@
/Definition of a toplevel function/
__funcdef__ ::= `func` var `(` comma_param `)` `:` type `{` stmt* `}` 
@


@
__stmt__ ::= 
         `let` param `=` exp `;` | /Variable declaration/
         var `=` exp `;` | /Assignment/
         `if` `(` exp `)` stmt [`else` stmt] | /if/
         `while` `(` exp `)` stmt | /while/
         `break` `;` | /break/
         `println` `(` exp `)` | /Printing something/
         `{` stmt* `}` | /Block/
         `return` [exp] `;` | /Return/
         exp `;` /Expression statements/
@


@
__struct_actual_param__ ::= var `:` exp
@


@
__struct_actual_params__ ::= [struct_actual_param (`,` struct_actual_param)*]
@


@
__primary_exp__ ::= 
                 i | var | /Integers and variables/
                 `true` | `false` | /Booleans/
                 `self` | /Instance on which we call a method/
                 `(` exp `)` | /Parenthesized expression/

                 /Creates a new instance of a struct/
                 `new` structname `{` struct_actual_params `}`
@


@
/Accessing a struct field or method/
__dot_exp__ ::= primary_exp (`.` var)*
@


@
__call_exp__ ::= dot_exp (`(` comma_exp `)`)*
@


@
__mult_exp__ ::= call_exp ((`*` | `/`) call_exp)*
@


@
__add_exp__ ::= mult_exp ((`+` | `-`) mult_exp)*
@


@
__less_than_exp__ ::= add_exp [`<` add_exp]
@


@
__equals_exp__ ::= less_than_exp [(`==` | `!=`) less_than_exp]
@


@
__exp__ ::= equals_exp
@


@
__program_item__ ::= structdef | traitdef | impldef | funcdef
@


@
__program__ ::= program_item* stmt* stmt* is the entry point
@

-}
module Docs.FormalSyntax where