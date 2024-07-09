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

## Automatic Drop Insertion
- If a bound value isn't used within it's binding scope then insert a drop at the end of the scope
  - Exception for function parameters which have a different special case
- ex:
```
:let x = 5 :in
:let y = (+ x 6) :in
:let (y1, y) = :split y :in
:let (y2, y3) = :split y :in
:let z = (* y1 y2) :in
z
```
x, y, y1, y, and y2 are all used but y3 isn't so a drop is inserted after z as follows

```
:let x = 5 :in
:let y = (+ x 6) :in
:let (y1, y) = :split y :in
:let (y2, y3) = :split y :in
:let z = (* y1 y2) :in
:let __res__ = z :in
:drop y3;
__res__
```

## Drop Typeclass

## UnRef
### Motivation - change drop so that it only ever returns unit (instead of something returning the value inside a ref)
### Needed additions - a way to get a value back out of a ref
- ex:
```
-- cell1 : ref<a, .5 * int>
let cell2 = :ref cell1 :in
let cell3 = :unref cell2 :in
...
```
cell2 can't be dropped because it contains a value that can't be dropped, since cell1 is a partial ref, so instead we use :unref to "peel back" the ref and get the value inside. This serves a similar function to pattern matching to get values out of product types, however we can't use that here since :unref can only be used on a full reference
