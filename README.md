# JSON Decode Exploration [![Build Status](https://travis-ci.org/zwilias/json-decode-exploration.svg?branch=master)](https://travis-ci.org/zwilias/json-decode-exploration)

I should really write some elevator-pitch style examples to include here. Oh
well.

Made with ❤️ and released under BSD3.

## Changelog

### 4.2.0

#### Added

- `Json.Decode.Exploration.check`: verify the value of a field while decoding
- `Json.Decode.Exploration.Pipeline`:
    - `checked` and `checkedAt`: verify values during decoding
    - `ignored` and `ignoredAt`: completely ignore values during decoding

Thanks to [@michaeljones](https://github.com/michaeljones) for the proposal.

#### Changed

Increased test coverage of `Json.Decode.Exploration.Pipeline`.

### 4.1.2

#### Fixed

`BadField` errors weren't properly traced through the stack.

#### Changed

Increase test coverage of `Json.Decode.Exploration`.
