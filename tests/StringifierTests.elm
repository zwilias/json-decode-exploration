module StringifierTests exposing (..)

import Expect
import Json.Decode.Exploration exposing (..)
import Test exposing (..)


formatWarning : String -> String
formatWarning =
    (++) "While I was able to decode this JSON successfully, I did produce one or more warnings:\n" >> String.trim


simpleWarning : Test
simpleWarning =
    test "simple warning" <|
        \_ ->
            let
                expectedError =
                    formatWarning """
  I encountered an unused value here. Are you sure you don't need this?

    []
"""
            in
            """ [] """
                |> decodeString (succeed "hi")
                |> strictResult
                |> Expect.equal (Err expectedError)


multipleWarnings : Test
multipleWarnings =
    let
        expectedError =
            formatWarning """
At path /foo

  I encountered an unused value here. Are you sure you don't need this?

    "bar"

At path /baz

  I encountered an unused value here. Are you sure you don't need this?

    "klux"
"""
    in
    test "multiple warnings" <|
        \_ ->
            """ { "foo": "bar", "baz": "klux" } """
                |> decodeString isObject
                |> strictResult
                |> Expect.equal (Err expectedError)
