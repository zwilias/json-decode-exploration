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


manualWarning : Test
manualWarning =
    let
        expectedWarning =
            formatWarning """
  null!

    null
"""
    in
    test "manual warning" <|
        \_ ->
            """ null """
                |> decodeString (warn "null!" (null Nothing))
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


failError : Test
failError =
    let
        expectedErrors =
            formatError """
  Failure all around!

    {}
"""
    in
    test "fail error" <|
        \_ ->
            """ {} """
                |> decodeString (fail "Failure all around!")
                |> errors
                |> Expect.equal (Just expectedErrors)


emptyOneOf : Test
emptyOneOf =
    let
        expectedErrors =
            formatError """
  I encountered a `oneOf` without any options.
"""
    in
    test "Empty oneOf" <|
        \_ ->
            """ null """
                |> decodeString (oneOf [])
                |> errors
                |> Expect.equal (Just expectedErrors)


multipleOneOfErrors : Test
multipleOneOfErrors =
    let
        expectedErrors =
            formatError """
  I encountered multiple issues:

    I expected a string here, but instead found this value:

      null

    I expected an integer number here, but instead found this value:

      null

    I expected a number here, but instead found this value:

      null

    I expected a boolean here, but instead found this value:

      null

    I expected an object here, but instead found this value:

      null

    I expected an array here, but instead found this value:

      null

    foo

      null

    I expected an object here, but instead found this value:

      null

    I expected an array here, but instead found this value:

      null
"""

        decoder : Decoder ()
        decoder =
            oneOf
                [ map (always ()) string
                , map (always ()) int
                , map (always ()) float
                , map (always ()) bool
                , map (always ()) (keyValuePairs string)
                , map (always ()) (list string)
                , map (always ()) (fail "foo")
                , isObject
                , isArray
                ]
    in
    test "Multiple oneOf errors" <|
        \_ ->
            """ null """
                |> decodeString decoder
                |> errors
                |> Expect.equal (Just expectedErrors)


missingField : Test
missingField =
    let
        expectedErrors =
            formatError """
  I expected an object with a field 'foo' here, but instead found this value:

    {}
"""
    in
    test "Missing field" <|
        \_ ->
            """ {} """
                |> decodeString (field "foo" string)
                |> errors
                |> Expect.equal (Just expectedErrors)


missingIndex : Test
missingIndex =
    let
        expectedErrors =
            formatError """
  I expected an array with index 0 here, but instead found this value:

    []
"""
    in
    test "Missing index" <|
        \_ ->
            """ [] """
                |> decodeString (index 0 string)
                |> errors
                |> Expect.equal (Just expectedErrors)


expectedNull : Test
expectedNull =
    let
        expectedErrors =
            formatError """
  I expected null here, but instead found this value:

    "foobar"
"""
    in
    test "Expected null" <|
        \_ ->
            """ "foobar" """
                |> decodeString (null "hi")
                |> errors
                |> Expect.equal (Just expectedErrors)


badJSON : Test
badJSON =
    let
        expectedErrors =
            formatError """
  Invalid JSON
"""
    in
    test "Bad JSON" <|
        \_ ->
            """ foobar """
                |> decodeString string
                |> strict
                |> Result.mapError errorsToString
                |> Expect.equal (Err expectedErrors)
