module Expect.Util exposing (..)

import Expect exposing (Expectation)


ok : a -> Result err a -> Expectation
ok =
    Ok >> Expect.equal


error : err -> Result err a -> Expectation
error =
    Err >> Expect.equal
