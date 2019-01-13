# purescript-generics-rep

[![Latest release](http://img.shields.io/github/release/purescript/purescript-generics-rep.svg)](https://github.com/purescript/purescript-generics-rep/releases)
[![Build status](https://travis-ci.org/purescript/purescript-generics-rep.svg?branch=master)](https://travis-ci.org/purescript/purescript-generics-rep)

Generic programming using an approach inspired by GHC.Generics.

This library requires version 0.10 of the PureScript compiler, or later.

## Installation

```
bower install purescript-generics-rep
```

## Introduction

PureScript's generics are supported by the `Data.Generic.Rep.Generic` type class:

```purescript
class Generic a rep | a -> rep where
  from :: a -> rep
  to :: rep -> a
```

There are three interesting things here:

- The `rep` type argument and associated functional dependency define a type function from user types (`a`) to their _representations_ (`rep`).
- `from` converts a regular value into the representation type.
- `to` converts a representation value back into a regular value.

This library provides standard representation types which can be used to represent any `data` types which can be expressed in PureScript code.

It is possible to write out `Generic` instances for our own types by hand, but doing so is very laborious. Instead, we can _derive_ instances by using the `derive` keyword:

```purescript
newtype Person = Person { name :: String, location :: String }

derive instance genericPerson :: Generic Person _
```

Note that the second type argument, which represents the representation type, is specified as a type wildcard. This is useful, because representation types can get quite large, so it is inconvenient to type them out by hand in deriving declarations.

### Show, Eq, Ord

The key insight regarding generics is this: if we can write a function which works with any of the standard representation types, then we implement the same function for any instance of `Generic`. We can even exploit type information in our implementation by using additional type classes to reflect the type information at runtime.

`purescript-generics-rep` provides helper functions for implementing common type classes from the Prelude:

- `genericShow` gives a default implementation of `show` from the `Show` class
- `genericEq` gives a default implementation of `eq` from the `Eq` class
- `genericCompare` gives a default implementation of `compare` from the `Ord` class
- `genericAppend` gives a default implementation of `append` from the `Semigroup` class
- `genericMempty` gives a default implementation of `mempty` from the `Monoid` class

Using these functions is as simple as dropping the generic implementation into your instances:

```purescript
instance showPerson :: Show Person where
  show = genericShow

instance eqPerson :: Eq Person where
  eq = genericEq

instance ordPerson :: Ord Person where
  compare = genericCompare

instance semigroupPerson :: Semigroup Person where
  append = genericAppend
```

### Performance Concerns

Generic deriving can be very convenient for code generation, but it comes with a performance penalty. Consider defining a `Show` instance using `genericShow` - instead of simply converting our data type directly to a `String`, we first convert it to the representation type, and then convert that representation into a `String`. Creating this intermediate structure comes with a cost.

Thankfully, the `generics-rep` approach means that we only need to perform a shallow copy of the data, up to the first data constructor or record, so in practice the performance cost is acceptable. In the case of [foreign-generic](https://github.com/paf31/purescript-foreign-generic), the benefits listed above usually outweight the performance cost, since we rarely need to parse or generate JSON in performance-critical sections of code in many applications.

## API Documentation

API documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-generics-rep).

## Related libraries

* The [documentation for the simple-json library](https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html) includes a tutorial for using this library.
* Generic deriving of codecs for `Foreign` values: [purescript-foreign-generic](https://github.com/paf31/purescript-foreign-generic)
* Generic deriving of codecs for purescript-argonaut: [purescript-argonaut-generic](https://github.com/purescript-contrib/purescript-argonaut-generic)