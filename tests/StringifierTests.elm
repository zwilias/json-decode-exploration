module StringifierTests exposing (..)

import Expect
import Json.Decode.Exploration exposing (..)
import Test exposing (..)


warnings : DecodeResult a -> Maybe String
warnings res =
    case res of
        WithWarnings w _ ->
            Just <| warningsToString w

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
                |> Expect.equal (Just expectedWarning)


errors : DecodeResult a -> Maybe String
errors res =
    case res of
        Errors e ->
            Just <| errorsToString e

        _ ->
            Nothing


formatError : String -> String
formatError =
    (++) "I encountered some errors while decoding this JSON:\n" >> String.trim


simpleError : Test
simpleError =
    let
        expectedErrors =
            formatError """
  I expected a string here, but instead found this value:

    {}
"""
    in
    test "simple error" <|
        \_ ->
            """ {} """
                |> decodeString string
                |> errors
                |> Expect.equal (Just expectedErrors)
