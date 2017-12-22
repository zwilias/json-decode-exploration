module StringifierTests exposing (..)

import Expect
import Json.Decode.Exploration exposing (..)
import Test exposing (..)


warnings : DecodeResult a -> Maybe Warnings
warnings res =
    case res of
        WithWarnings w _ ->
            Just w

        _ ->
            Nothing


formatWarning : String -> String
formatWarning =
    (++) "While I was able to decode this JSON successfully, I did produce one or more warnings:\n" >> String.trim


simpleWarning : Test
simpleWarning =
    test "simple warning" <|
        \_ ->
            let
                expectedWarning =
                    formatWarning """
  Unused value:

    []
"""
            in
            """ [] """
                |> decodeString (succeed "hi")
                |> warnings
                |> Maybe.map warningsToString
                |> Expect.equal (Just expectedWarning)


multipleWarnings : Test
multipleWarnings =
    let
        expectedWarning =
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
                |> warnings
                |> Maybe.map warningsToString
                |> Expect.equal (Just expectedWarning)


nestedWarnings : Test
nestedWarnings =
    let
        expectedWarning =
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
                |> warnings
                |> Maybe.map warningsToString
                |> Expect.equal (Just expectedWarning)
