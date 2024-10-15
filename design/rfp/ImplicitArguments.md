### Motivation
Currently* typeclasses (currently ``Callable`` and soon ``Drop``) are supported but only for things special cased by the compiling
the goal of implicit arguments is to make it possible for users to write their own functions with typeclass constaints

### Declaring a function with implict arguments
Implicit arguments are declared using ```{ implicit }``` before a function type to use multiple implicits include them as 
a tuple ex: ```{ (Callable a b f, Drop f) }```
- Mangled names are generated for the implicit parameters inside of the function, these names are "unspeakable" since they contain
  colons which are not recognized by the parser, and the naming schema is intentional left as an implementation detail that can change
  at any time. This means that they can only be used a implicit paremeters to other functions, or used explicitly by giving them a name using
  summom (described below)
- If the implicit parameter is a tuple then it is unpacked and each of it's individual fields are also given names (in addition to the tuple itself having a name generate for it), this happens recursively for nested tuples

ex:
```hs
:let
map :: { Callable a b f } (f, List a) -> List b
map = ...
:in
```
An alternative syntax is available so that not as many type parameters need to be use:
```hs
:let
map :: ({ Callable a b }, List a)->List b
map = ...
:in
```
The alternate syntax also supports multiple constraints on one argument
```hs
:let
map :: ({ (Callable a b, Drop) }, List a)->List b
map = ...
:in
```
this is treated the same as the above

## Restrictions on implicit arguments
Any type can be used as an implicit argument however, if the implicit argument is a fractional type then it's fraction at the end of the function must be the same as it fraction that the beggining. More generally the type of an implicit parameter must be the same at the end of the function as it is a the beginning but in practice this only matters for fractional types

### Implicit Declarations
In addition to implicit arguments it is also possible to declare implicit bindings using the form
```hs
:implicit TYPE = EXPR :in
```
which declares an implicit variable of type ``TYPE``, this variable is given a name using the same generation algorithm as with implicit arguments and can also be used for implicit parameters or with summon just like implicit arguments are

### Summon
Allows you to give an implicit argument with an "unspeakable name" a normal name so that you can use it
```hs
:let
addImplicit :: {Num} (Num)->Num
addImplicit b = :summon Num :as a :in a + b
:in
```
Restrictions:
- If the summoned value is a fraction type it must have the same fraction at the end of the summon block,
  this prevents you from storing an implicit arguments since they are meant to always be threaded through.

### Implicit Argument Inference




### Open questions