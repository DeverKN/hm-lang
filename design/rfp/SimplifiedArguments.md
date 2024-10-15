NOTES: This is just a place to store my thought about a potential future feature, there are no plans of implimenting this now or anytime soon

### Situation

Currently arguments have to be explicitly threaded through functions, this creates a lot of extra boilerplate since every argument that isn't dropped must be returned. This overhead is created both for the function creator, who has to explicitly return all the parameters, but also for the function call since they must rebind all the parameters. This also obscures the true nature of what a function does. For example, what does a this function do?
```hs
func1 :: (<frac1 * (Ref Num addr1)>, <frac2 * (Ref Num addr2)>) -> (Num, (<frac1 * (Ref Num addr1)>, <frac2 * (Ref Num addr2)>))
```
It adds two numbers stored in references and returns the result (or subtract or multiplies, you get the picture) however it's hard to tell this from the function signature since it's so cluttered.

### Potential Solutions

1. Out Parameter Types
```hs
func1 :: (in: <frac1 * (Ref Num addr1)> out: <frac1 * (Ref Num addr1)>, in: <frac2 * (Ref Num addr2)> out: <frac2 * (Ref Num addr2)>)->Num
```

2. Sugar
This is the same as the above (inout just means in and out are the same)
```hs
func1 :: (inout: <frac1 * (Ref Num addr1)>, inout: <frac2 * (Ref Num addr2)>)->Num
```

This is also the same as the above (parameters are assumed to be inout unless explicity annotated)
```hs
func1 :: (<frac1 * (Ref Num addr1)>, <frac2 * (Ref Num addr2)>)->Num
```

This:
```hs
func1 :: (<frac1 * (Ref Num addr1)>, consumed: <frac2 * (Ref Num addr2)>)->Num
```
is the same as this:
```hs
func1 :: (inout: <frac1 * (Ref Num addr1)>, in: <frac2 * (Ref Num addr2)> out: ())->Num
```
```:consumed TY``` is just shorthand for ```:in TY out: ()```

We could use some other syntax instead of in out:
example:
```:in <frac1 * (Ref Num addr1)> :out <frac1 * (Ref Num addr1)>```
or
```<frac1 * (Ref Num addr1)> :to <frac1 * (Ref Num addr1)>```
or
```<frac1 * (Ref Num addr1)> |> <frac1 * (Ref Num addr1)>```
but while these are all shorter I think they're harder to understand (especially the last option). The middle one could maybe be used since it's pretty easy to tell what comes before and what's after but it's hard to understand unless you already know what :to means whereas :in and :out are pretty self explanitory. Another option is to just elide the :in and allow something like
```<frac1 * (Ref Num addr1)> :out <frac1 * (Ref Num addr1)>```

2. Named parameter types

### Aside - Generic Placeholders
The function above have a bunch of generic parameters (four to be exact). They're only ever used once so the names don't matter but the programmer still has to come up with individual names for each one. Instead of giving them explicit names instead we could use a placeholder (_) and let the compiler generate names for them. This is fine since they are only ever actually used in one place and primarily serve to let the function work with any fraction or address
```hs
func1 :: (<_ * (Ref Num _)>, <_ * (Ref Num _)>) -> Num
```

This could be written as:
```hs
func1 :: (<a * (Ref Num b)>, <c * (Ref Num d)>) -> Num
```
However in both cases the names provide no information and the first makes it clearer that we don't care about the parameters

Placeholders can only be used for parameters that only appear once since every instance of _ is replaced with a different generated name


We could even make it more concise like this
```hs
func1 :: (<Ref Num>, <Ref Num>) -> Num
```
Here a fractional type without a fraction is given an implicit fraction parameter and similarly a ref with no address is given an implicit forall, but this is probably to hard to understand (despite the terseness benefits)