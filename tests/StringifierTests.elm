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
  Unused value:

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

  Unused value:

    "bar"

At path /baz

  Unused value:

    "klux"
"""
    in
    test "multiple warnings" <|
        \_ ->
            """ { "foo": "bar", "baz": "klux" } """
                |> decodeString isObject
                |> strictResult
                |> Expect.equal (Err expectedError)


nestedWarnings : Test
nestedWarnings =
    let
        expectedError =
            formatWarning """
At path /root/0

  Unused value:

    "foo"

At path /root/1

  Unused value:

    "bar"
"""
    in
    test "nested warnings" <|
        \_ ->
            """
             { "root":
               [ "foo"
               , "bar"
               ]
             }
             """
                |> decodeString (field "root" isArray)
                |> strictResult
                |> Expect.equal (Err expectedError)
