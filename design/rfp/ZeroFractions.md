NOTE: This document describes design choices that were already made but it exists to help explain why they were made

### Motivation:

```hs
read :: <addr, frac * Ref a> -> a
read ref = !ref
```

This code looks fine, but it is wrong since you could pass <addr, 0 * Ref Int> as the argument. This would
be unsafe since there could be another full reference being mutated elsewehere. It's obvious

## Possible Solutions

1. Add constraints onto generic fractions
The above could be rewritten as (made up notation)
```hs
read :: <addr, (frac > 0) * Ref a> -> a
read ref = !ref
```
This solves the problem of zero fractions but adds alot of extra complexity. Something like an SMT solver would need to be added to the type checker ensure the constraints are upheld. This would also probably be overkill since most of the constraints would just be (> 0). 

This there are really three types of ref:
Fraction = 1      <- can be read to and written to
1 > Fraction > 0  <- can only be read
Fraction = 0      <- nothing* can be done with it

The first constraint (Fraction = 1) can just be expressed as 
```hs
<addr, 1 * Ref a>
```
with no need for complex constraints

2. Add a NonZero reference type
Since the only constraint we ever really care about it that the fraction is > 0 you could instead do something like:
```hs
read :: (NonZero <addr, frac * Ref a>) -> a
read ref = !ref
```
or
```hs
read :: NonZero frac => <addr, frac * Ref a> -> a
read ref = !ref
```
(the notation doesn't really matter)
This would probably be easier to implement than a full constraint system and solves the problem of having generic fractional references that can always be written to but it clutters up type signatures and also raises the question of when you would ever not use NoneZero

3. Don't have zero references

Zero references are useless. You can't write to them, you can't read from them the only thing you can do is merge them with another reference however this is just the identity function since the fraction of the other reference won't change. Zero references are similar to null pointers in that they look like a reference at the type level but can't actually be used for anything. They aren't as bad as null pointers the typechecker forbids writing to or reading from them, but they still don't do anything. Zero references are effectively just a synonym for the unit type, since they can't actually be used for anything. The one purpose they serve is in situations like this:
```hs
data Node a frac = Node a (<#unique, frac * Ref (Node a any))
```
(this example is contrived)
where you could use a zero fraction ref as a "placeholder" but much like using null as a placeholder this is a bad pattern and instead you should use something like ```Maybe``` to represent the fact that sometimes there isn't a pointer
```hs
data Node a frac = Node a (Maybe <#unique, frac * Ref (Node a any))
```

In the end I went with option 3 since it makes a lot of other stuff simpler since you can assume that any generic fraction is readable, and zero references can be emulated using ```Maybe``` it also has the benefit of not cluttering things up with a bunch of zero fraction references that can't actually do anything