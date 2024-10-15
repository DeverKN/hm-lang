### Motivation

Currently everything with a fraction also has an abstract address. Abstract addresses are necessary for Refs to prevent the merging of two refs that actually point to different things, the abstract address serving to represent the runtime address of the pointer. However, for other types like Tuples, ADTs, or Closures all have a fraction but they don't have a runtime address for the abstract address to represent and more importantly they don't need addresses to prevent issues with mergining. A Tuple can be merged with any other tuple with exactly the same fields without there being any issues.

### Solution

1. Seperate Addresses from Fraction
- Not everything that has a fraction has an address so the fraction type is changed to just be  ```<FRAC * TYPE``` instead of ```<ADDRESS, FRAC * TYPE>``` 
- The address paramter is instead move to refs since those are the only things that actually have a runtime fraction. Refs are now ```Ref TYPE ADDRESS``` rather than just ```Ref TYPE```

### Examples

before:
```hs
-- a :: <#exists, 1 * Ref Num>
let a = :ref 5 :in
-- tuple :: <#exists1, 1 * (<exists2, 1 * Ref Num>, Num)>
let tuple = (a, 5) :in
```

after:
```hs
-- a :: <1 * (Ref Num #exists)>
let a = :ref 5 :in
-- tuple :: <1 * (<1 * (Ref Num #exists)>, Num)>
let tuple = (a, 5) :in
```

### Changes
- Remove the address parameter from normal fractional types
- Add an address parameter to the Ref type