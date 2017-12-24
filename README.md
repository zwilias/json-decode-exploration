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
