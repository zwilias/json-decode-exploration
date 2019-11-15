# JSON Decode Exploration [![Build Status](https://travis-ci.org/zwilias/json-decode-exploration.svg?branch=master)](https://travis-ci.org/zwilias/json-decode-exploration) [![codecov](https://codecov.io/gh/zwilias/json-decode-exploration/branch/master/graph/badge.svg)](https://codecov.io/gh/zwilias/json-decode-exploration)

I should really write some elevator-pitch style examples to include here. Oh
well.

Made with ❤️  and released under BSD3. My eternal thanks to
[@michaeljones](https://github.com/michaeljones) for his invaluable feedback and
continued discovery of edge-cases and bugs!


## Contributing

Contributions are super-welcome. For larger changes, please do contact me first
on Slack so we can talk things through. For bugs or incorrect behaviour, open an
issue so we can quickly hash out a decent solution, and get everyone on the same
wavelength before any code is written. Contribution is collaboration, which
can't happen in a void.

We have a decent test-suite, but the more the merrier. Additionally, every
example in the docs is verified using `elm-verify-examples`. To make sure
everything still works, `npm test` will check both.

---

## Changelog

### 6.0.0

Propagation through errors, stripping, and unused fields/indexes versus unused values.

In previous releases, this library did not differentiate between "the decoder
does not require a field to exist" and "the decoder did not inspect the value of
this field", and likewise for indexes in JSON arrays.

In order to make that differentation possible, the `Warning` type was expanded
with `UnusedField String` and `UnusedIndex Int` constructors.

This release also brings propagation of usage-tracking through errors. Consider
a decoder for a custom type like `type Foo = Foo | Bar | Baz` like so:

```elm
foo : Decoder Foo
foo =
    Decode.andThen
        (\fooString ->
            case fooString of
                "foo" -> Decode.succeed Foo
                "bar" -> Decode.succeed Bar
                _ -> Decode.fail "invalid"
        )
        Decode.string
```

Now imagine using that decoder to decode a field, and defaulting the value to
`Foo` if we failed to decode the value: `Decode.field "hi" (Decode.oneOf [ foo,
Decode.succeed Baz ])`.

If the input were `{"hi": "there"}`, the library would previously have issued an
`UnusedValue` warning for the `"hi"` string. However, we did, in fact, inspect
its value in order to arrive at our final result. This release will _not_ issue
a warning when this decoder results in `Baz`.

Finally, the most exciting feature in this release, with thanks for
@dillonkearns for coming up with the motivating use-case and helping think
things through: stripping JSON values! The new `stripString` and `stripValue`
functions allow "minimalizing" input to only contain that information which is
required to have the decoder produce the same value without any warning after
stripping.

As an example use-case, this can be used during development to show what a JSON
should minimally be stripped down to when decoding succeeds with warnings.

### 5.0.1

Elm 0.19.0 compatibility 🎉

#### Changed

- `Json.Decode.Exploration.check` no longer includes the expected value in the error
- `Json.Decode.Exploration.Pipeline.checked` no longer includes the expected value in the error

### 5.0.0

#### Added

- `Json.Decode.Exploration.Located`

   A module that describes the location of an error or warning within the JSON.
   Using a single datatype to represent these means we get to treat them in a
   uniform way.

- `Json.Decode.Exploration`:
   - `strict : DecodeResult a -> Result Errors a`: interpret a decoderesult in
     a strict manner; converting warnings to errors.
   - `warningsToString` and `errorsToString`: Turn the machine readable `Errors`
     and `Warnings` into human readable `String`s.

     Together with `strict`, this can be used to create a `Decoder a -> String
     -> Result String a` function for compatibility with the core API.
   - `warn : String -> Decoder a -> Decoder a`: attach a warning to a decoder.
   - `ExpectedType`: type to represent the expected type for errors.

#### Changed

- `Json.Decode.Exploration`:
   - `Warnings` and `Errors` are now defined in terms of the `Located` type.
   - `Error` now has machine readable "expected type" errors.
   - `Warning` now has a `Warning String Value` constructor for arbitrary
     warnings.

### 4.3.0

#### Added

- `Json.Decode.Exploration`:
    - `isObject` and `isArray`: ascertainments about structure without using
      contained values

#### Changed

- `Json.Decode.Exploration.Pipeline`:
    - `optional` and `optionalAt` now use `isObject` to prevent marking unused
      contained values as having been used.

### 4.2.1

#### Changed

- `Json.Decode.Exploration.Pipeline.optional` and `optionalAt` now error when
  used on something that is not an object. The clue is that each field in the
  path is optional in the existential sense - if it simply does not exist, we
  dutifully use the provided fallback. However, if a field exists but does not
  point to an object, we now error out instead.
- as a side effect, both of the above now correctly mark empty objects as having
  been used, rather than giving a warning when using `optional` on an empty
  object.

### 4.2.0

#### Added

- `Json.Decode.Exploration.check`: verify the value of a field while decoding
- `Json.Decode.Exploration.Pipeline`:
    - `checked` and `checkedAt`: verify values during decoding
    - `ignored` and `ignoredAt`: completely ignore values during decoding

#### Changed

Increased test coverage of `Json.Decode.Exploration.Pipeline`.

### 4.1.2

#### Fixed

`BadField` errors weren't properly traced through the stack.

#### Changed

Increase test coverage of `Json.Decode.Exploration`.
