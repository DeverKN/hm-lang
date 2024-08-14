# TODO

## Type Classes
1. Implicit Arguments
2. Synthesization of Implicit Arguments
3. Syntax Sugar for Type Classes

## Fn Typeclass
### Three Categories of Function
* Fn - only reads captured values and doesn't change enviroment
  - Can be called 
* FnMut - reads, writes, and merges captured values but doesn't change environment
  - Can only be called with a full reference
* FnOnce - reads, writes, and merges captured values and changes environment
  - Can only be called once (and must consume/return the entire environment)
### Hierarchy 
* FnOnce -> FnMut -> Fn

### Related - Closures
- Change arrow types to be functions without an env and create a closure type
- exs:
```ml
<#unique, 1 * ([] () -> int)>
```
becomes
```ml
(() -> int)
```

and 
```ml
<#unique, 1 * (a () -> int)>
```
becomes
```ml
<#unique, 1 * (Closure a (() -> int))>
```
- Implementation notes
* typechecker determines if a function declaration becomes a regular arrow or a closure
* arrows can still capture non-fractional environment values, but if it captures any fractional values then it becomes a closure
* there is also a closure hierarchy: ClosureOnce -> ClosureMut -> Closure
  - Ideally compiler can figure out what type of closure a function declaration becomes but not 100% sure how to implement yet
  - If env is different at end then ClosureOnce
  - If anything other than normal read is done then ClosureMut

## Automatic Drop Insertion
- If a bound value isn't used within it's binding scope then insert a drop at the end of the scope
  - Exception for function parameters which have a different special case
- ex:
```ml
:let x = 5 :in
:let y = (+ x 6) :in
:let (y1, y) = :split y :in
:let (y2, y3) = :split y :in
:let z = (+ y1 y2) :in
z
```
x, y, y1, y, and y2 are all used but y3 isn't so a drop is inserted after z as follows

```ml
:let x = 5 :in
:let y = (+ x 6) :in
:let (y1, y) = :split y :in
:let (y2, y3) = :split y :in
:let z = (+ y1 y2) :in
:let __res__ = z :in
:drop y3;
__res__
```

## Drop Typeclass

## Specialized Drop
* Specific drop functions for each type
- ex:
```
-- drop_int : Int -> ()
-- drop_string : String -> ()
-- drop_tuple : {a -> ()} -> {b -> ()} -> (a,b) -> ()
```

## UnRef
### Motivation - change drop so that it only ever returns unit (instead of something returning the value inside a ref)
### Needed additions - a way to get a value back out of a ref
- ex:
```ml
-- cell1 : ref<a, .5 * int>
:let cell2 = :ref cell1 :in
:let cell3 = :unref cell2 :in
...
```
cell2 can't be dropped because it contains a value that can't be dropped, since cell1 is a partial ref, so instead we use :unref to "peel back" the ref and get the value inside. This serves a similar function to pattern matching to get values out of product types, however we can't use that here since :unref can only be used on a full reference

## Type Synonyms
- ex:
```ml
:type Unique addr a = <addr, 1 * a> :in
```

## Improve unification algorithm