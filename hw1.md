# HW 1
## 1
For each of the following, investigate the OCaml manual to find out what OCaml has, and explain in your own words how OCaml compares to another language of your choice (perhaps Java or Python). Make sure to try out examples in the toplevel.

- primitive types
  Ocaml has:
  - Basics: bools, immutable strings, characters, tuples, arrays, lists, functions, records, variants, polymorphic types, recursive types.
    mostly the same as Haskell except for Generalize Algebraid Data Types(GADTs) are part of the language and not an extension.
    Ocaml preferes Modules to Haskells typeclasses (Automatic derivation of typeclass instances is not built into Ocaml, achieved via plugin)
    Main method not forced in IO monad (as Haskells is)
- operators
- assertions
## 2 
How do OCaml if expressions compare and contrast with if commands in imperative languages, such as Java and Python? And with ternary operators? (If you're not already familiar with ternary operators in those languages, now's a good time to look them up!)

## 3
Do the exercises named values, if, assert, and more fun from the exercises section at the end of chapter 2 of the 3110 textbook.

## 4
When using the OCaml build system, what files get created, and where are they are located? Experiment as necessary to find out. Note that section 2.1 of the textbook will be helpful.
