### Motivation:

Currently

## Short Fixes

```hs
type RawFunc f = <#exists, [], f>
map :: (RawFunc (a -> b)) -> (List a) -> (List b)
```
this could work as a quick fix but there are two issues:
1. The default should be "regular" non-closure functions since those are much easier to work with, this solution would make the default closures
and then require extra work to get it into the easier to use form
2. `RawFunc (a -> b)` isn't valid because `a -> b` isn't a wellformed type by itself, it must be in the form `<ADDR, ENV, a -> b>`

## Plan for now:
### Add a "raw" function type:
- ex. (a -> a)
- This is also an opportunity to fix function type syntax to be more accurate since they aren't actually curried
- ex. use ((a, b) -> (b, a)) instead of (a -> b -> (b, a))
### Add a builtin closure type that wraps a tuple or value and a function:
- This type DOES have a constructor
- `Closure env args ret = Closure <#exists, 1 * env> ((<#exists2, 1 * env>, args) -> (<#exists2, 1 * env>, ret))`
- Also builtin function for calling closures
```hs
let callOwnedClosure :: <addr, 1 * (Closure env args ret)> -> args -> (ret, <addr, 1 * (Closure env args ret)>)
    callOwnedClosure closRef args = let clos = :drop closRef in
                                    let ((newEnv, ret), func) = (func (env, args)) in 
                                    (ret, (Closure newEnv func))
```
- Only owned closures can be called since they treat the environment like it's owned (this is subject to change in the future)
- When a function is declared the compiler automatically assigns it either Closure or (->) based on if it captures any fractional types
- This lets us get rid of special handling for closures (ex. unClos since now they can just be deconstructed like any other type)
- Offer special syntactic sugar for Closures
- `|int| a -> a := Closure int a a`
- `|int, str| (a, a) -> a := Closure (int, str) (a, a) a`
- Environments are now represented as tuple (or a value)
### Add a ClosurePartial type
- if a function writes to an of its captured variables than it can only safely be called with a full copy of it's environment
(and thus a full reference to itself). However, if it only reads (or acquires) captured values then it can safely be called from
a partial reference, this is represented by the ```ClosurePartial``` type
- It has a builtin function that can call a closure even with a partial reference
- It SHOULD have a constructor but idk if it's representable
- `ClosurePartial env args ret = ClosurePartial <#exists, 1 * env> (forall addr frac. ((<addr, frac * env>, args) -> (<addr, frac * env>, ret)))`
- Currently this isn't representable since there isn't support for rank 2 types (needed for the nested forall)
```hs
let callClosurePartial :: <addr, frac * (ClosurePartial env args ret)> -> args -> (ret, <addr, frac * (ClosurePartial env args ret)>)
    callClosurePartial closRef args = let clos = :drop closRef in
                                      let ((newEnv, ret), func) = (func (env, args)) in 
                                      (ret, (Closure newEnv func))
```

### Determining the type for a function

                        Does it capture any fractional types?
                        /                                   \
                      No                                    Yes
                      |                                      |
                Raw arrow type  (->)                   Does it return/store all or part of any of them?
                                                       (alternatively: are the fractions for any of them 
                                                       different at the end of the function than when it 
                                                       started?)
                                                       /                                                \
                                                     Yes                                                No
                                                      |                                                 |
                                        Are ALL of the fractions of                           Does it need to capture a full reference
                                        captured values zero at the                           to any of them (due to writing to them or
                                        end of the function?                                  or passing them to a function that expects
                                        /                   \                                 a full reference)?
                                       No                   Yes                               /                                        \
                                       |                     |                               No                                        Yes
                                     Error              ClosureOnce                          |                                          |
                                                                                          ClosurePartial                             Closure

### Callable typeclass 
- Given `f :: f, a :: a, b :: b` when the compiler sees `f (a,b)` it looks for an instance of `Callable (a,b) c f` and uses `c` as the return type
- There is a special builtin instance of `Callable` for all arrow types that takes the form `Callable a (b, a -> b) (a -> b)`
```hs
instance Callable args ret (args -> ret) = {- # COMPILER MAGIC # -}
in
```
- There is also an instance for all closure types, given by

```hs
instance args ret Callable <addr, 1 * (Closure env args ret)> = callClosure
instance args ret Callable <addr, frac * (PartialClosure env args ret)> = callPartialClosure
in
```

### ClosureOnce
Represents a Closure that can only be used once, used to implement currying
Curried functions can't be implement using normal closures otherwise you
could clone values using something like this
```hs
makeTuple :: a -> b -> (a, b)
let makeTuple a b = ... in
    let cell = :ref 0 in
    let curried = makeTuple cell in
    let tupleA = curried () in
    let tupleB = curried () in
    ...
    -- tupleA and tupleB now both have a full copy of a
```

### Tuple intrinsic
Since Tuples are now used for the arguments to multi-argument functions the tuple constructor can't be normal multi argument function (since that would be self
referential). One fix would be to use curried functions, however this would require something like the ClosureOnce described above, the simpler fix is to just
special case the tuple constructor so that instead of being a function it's a builtin

ex:
instead of
```hs
let pair = (1,2)
```
compiling to
```hs
let pair = tuple:2 1 2 
```
it becomes (made up syntax)
```hs
let pair = :tuple_intrinsic_2 1 2
```

```hs
instance Callable (ClosureOnce env args return) args return
```

### Arrow Syntax Sugar
```hs
Closure (a,b) (a,b) (c) === |a,b| (a,b) -> c
ClosureOnce (a,b) (a,b) (c) === |a,b| (a,b) -!> c --bikeshed this
ClosurePartial (a,b) (a,b) (c) === |a,b| (a,b) -?> c --bikeshed this
Callable f (a,b) c === call(a,b)->c --bikeshed this
```

#### Out of Scope (For Now)
- Instance inference - the only place where instances will be implicity inferred from the environment is when looking for an instance `Callable f (a,b) c` for `f (a,b)` everywhere else (ex. function parameters) they must be explicitly passed

## Required Changes:
1. Raw function types
- Parser needs to be updated to support this
2. Builtin Closure type
3. Builtin ClosureParital type
4. Builtin ClosureOnce type
5. Builtin tuple constructor
- The tuple constructor needs to be special cased
- 2 changes
- Add special ```tuple<N>constructor``` AST node for the runtime
- Special case tuple constructors when doing constructor lookup for pattern matching (tbh we already kind of do this
but need to make it even specialer)
6. Rank2 Polymorphism (support for nested foralls, ex: (forall a. a -> a) -> a -> a)
- Parser needs to be updated to support the new syntax
- Typechecker needs to be updated to check them (might already work, idk)
7. Function argument patterns
- Since multi-argument functions are now of the form (Num, Num)->Num function definitions need to be updated
- Support pattern matching at the function argument level
- ex:
```hs
    :let
    add :: (Num,Num) -> Num
    add (a,b) = a + b 
    :in
```
at base this would only allow one case but this is a chance to potentially allow multi-case functions
- ex:
```hs
    :let
    add :: (Num,Num) -> Num
    add (a,0) = a
    add (a,b) = add1 (add (a, sub1 b))
    :in
```
- this would require parser retooling but could be parsed as:
```hs
:let
<IDENTIFIER> :: <TYPE> -> <TYPE>
(<IDENTIFIER> <PATTERN> = <EXPR>)+
:in
```
8. Typeclasses
This is one of the biggest changes, it requires:
- 
Out of scope for now:
- Type class constraints on functions/implicit arguments
- For now type class instances must be passed around like normal arguments

## 