module Example exposing (..)

import Expect.Util as Expect
import Fuzz exposing (Fuzzer)
import Json.Decode.Exploration as Decode
    exposing
        ( Decoder
        , Error(..)
        , ErrorType(..)
        , MisMatch(..)
        , Node(..)
        , StructuralIssue(..)
        )
import Json.Encode as Encode exposing (Value)
import Test exposing (..)


invalidJson : Test
invalidJson =
    test "invalid json gives structural issue" <|
        \_ ->
            Decode.decodeString Decode.value "{"
                |> Expect.error InvalidJSON


intDecoder : Test
intDecoder =
    describe "int decoder"
        [ test "decodes 5" <|
            \_ ->
                Decode.decodeString Decode.int "5"
                    |> Expect.ok 5
        , test "failure gives sane error" <|
            \_ ->
                Decode.decodeString Decode.int """ "foo" """
                    |> Expect.error (Error <| TypeMisMatch ExpectedInt)
        , fuzz intVal "decodes random integers" <|
            \( int, json ) ->
                Decode.decodeValue Decode.int json
                    |> Expect.ok int
        ]


intVal : Fuzzer ( Int, Value )
intVal =
    Fuzz.int |> Fuzz.map (\x -> ( x, Encode.int x ))


fieldDecoder : Test
fieldDecoder =
    describe "field"
        [ test "decodes contents" <|
            \_ ->
                """ { "foo": 5 } """
                    |> Decode.decodeString (Decode.field "foo" Decode.int)
                    |> Expect.ok 5
        , test "missing field is error" <|
            \_ ->
                """ {} """
                    |> Decode.decodeString (Decode.field "foo" Decode.int)
                    |> Expect.error (Error <| Structural <| ExpectedField "foo")
        , test "adds context for nested errors" <|
            \_ ->
                """ { "foo": "bar" } """
                    |> Decode.decodeString (Decode.field "foo" Decode.int)
                    |> Expect.error
                        (InContext
                            { path = [ Field "foo" ]
                            , error = TypeMisMatch ExpectedInt
                            }
                        )
        ]


listDecoder : Test
listDecoder =
    describe "list"
        [ test "decodes a list of things" <|
            \_ ->
                """ [ 1, 2, 3 ] """
                    |> Decode.decodeString (Decode.list Decode.int)
                    |> Expect.ok [ 1, 2, 3 ]
        , test "fails if its not a list" <|
            \_ ->
                """ null """
                    |> Decode.decodeString (Decode.list Decode.int)
                    |> Expect.error (Error <| Structural <| ExpectedArray)
        , test "retains path if inner decoder fails" <|
            \_ ->
                """ [ 1, "foo", 3 ] """
                    |> Decode.decodeString (Decode.list Decode.int)
                    |> Expect.error
                        (InContext
                            { path = [ Index 1 ]
                            , error = TypeMisMatch ExpectedInt
                            }
                        )
        , test "multiple errors accumulate" <|
            \_ ->
                """ [ "foo", 2, "bar" ] """
                    |> Decode.decodeString (Decode.list Decode.int)
                    |> Expect.error
                        (Error <|
                            Multiple
                                [ InContext { path = [ Index 0 ], error = TypeMisMatch ExpectedInt }
                                , InContext { path = [ Index 2 ], error = TypeMisMatch ExpectedInt }
                                ]
                        )
        ]


atDecoder : Test
atDecoder =
    describe "at"
        [ test "decodes contents" <|
            \_ ->
                """ { "foo": 5 } """
                    |> Decode.decodeString (Decode.at [ "foo" ] Decode.int)
                    |> Expect.ok 5
        , test "decodes nested contents" <|
            \_ ->
                """ { "foo": { "bar": 5 } } """
                    |> Decode.decodeString (Decode.at [ "foo", "bar" ] Decode.int)
                    |> Expect.ok 5
        ]


complicated : Test
complicated =
    let
        decoder : Decoder (List Int)
        decoder =
            Decode.field "field" (Decode.list (Decode.field "cnt" Decode.int))
    in
    describe "complicated"
        [ test "succeeds" <|
            \_ ->
                """ { "field": [ { "cnt": 1 }, { "cnt": 2 }, { "cnt": 3 } ] } """
                    |> Decode.decodeString decoder
                    |> Expect.ok [ 1, 2, 3 ]
        , test "fails with context" <|
            \_ ->
                """ { "field": [ { "cnt": 1 }, { "cnt": null }, { "cnt": 3 } ] } """
                    |> Decode.decodeString decoder
                    |> Expect.error
                        (InContext
                            { path = [ Field "field", Index 1, Field "cnt" ]
                            , error = TypeMisMatch ExpectedInt
                            }
                        )
        ]


oneOfDecoder : Test
oneOfDecoder =
    describe "oneOf"
        [ test "Empty oneOf fails" <|
            \_ ->
                """ [] """
                    |> Decode.decodeString (Decode.oneOf [])
                    |> Expect.error (Error (Multiple []))
        , test "first success wins" <|
            \_ ->
                """ [] """
                    |> Decode.decodeString
                        (Decode.oneOf
                            [ Decode.succeed "first"
                            , Decode.succeed "second"
                            ]
                        )
                    |> Expect.ok "first"
        , test "success after failure still wins" <|
            \_ ->
                """ [] """
                    |> Decode.decodeString
                        (Decode.oneOf
                            [ Decode.fail "nope"
                            , Decode.succeed "sure!"
                            ]
                        )
                    |> Expect.ok "sure!"
        , test "If we only have failure, we get all the failures" <|
            \_ ->
                """ [] """
                    |> Decode.decodeString
                        (Decode.oneOf
                            [ Decode.fail "first"
                            , Decode.fail "second"
                            ]
                        )
                    |> Expect.error
                        (Error
                            (Multiple
                                [ Error (Custom "first")
                                , Error (Custom "second")
                                ]
                            )
                        )
        ]


type Recursive
    = Val String
    | Branch (List Recursive)


lazyDecoder : Test
lazyDecoder =
    let
        decoder : Decoder Recursive
        decoder =
            Decode.oneOf
                [ Decode.map Val Decode.string
                , Decode.map Branch (Decode.list <| Decode.lazy (\_ -> decoder))
                ]
    in
    test "lazy" <|
        \_ ->
            """ [ [ "foo", "bar" ], "foo" ] """
                |> Decode.decodeString decoder
                |> Expect.ok
                    (Branch [ Branch [ Val "foo", Val "bar" ], Val "foo" ])


type alias TripRecord =
    { foo : String, bar : String, baz : String }


map3Decoder : Test
map3Decoder =
    let
        decoder : Decoder TripRecord
        decoder =
            Decode.map3 TripRecord
                (Decode.field "foo" Decode.string)
                (Decode.field "bar" Decode.string)
                (Decode.field "baz" Decode.string)
    in
    test "map3 retains all the errors" <|
        \_ ->
            """ {} """
                |> Decode.decodeString decoder
                |> Expect.error
                    (Error
                        (BadMap
                            (Error
                                (BadMap
                                    (Error (Structural (ExpectedField "foo")))
                                    (Error (Structural (ExpectedField "bar")))
                                )
                            )
                            (Error (Structural (ExpectedField "baz")))
                        )
                    )
