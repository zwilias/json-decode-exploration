module SimpleTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode.Exploration as Decode
    exposing
        ( DecodeResult(..)
        , Decoder
        , Error(..)
        , Errors
        , Warning(..)
        , Warnings
        )
import Json.Decode.Exploration.Located exposing (Located(..))
import Json.Encode as Encode
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Native.TestHelpers
import Test exposing (..)


unusedSimpleValues : Test
unusedSimpleValues =
    [ Encode.string "foo"
    , Encode.int 12
    , Encode.float 0.1
    , Encode.null
    , Encode.bool True
    , Encode.list []
    , Encode.object []
    ]
        |> List.map unusedValueTest
        |> describe "Unused simple values"


unusedValueTest : Encode.Value -> Test
unusedValueTest value =
    let
        warnings : Warnings
        warnings =
            Nonempty (Pure <| UnusedValue value) []
    in
    test (Encode.encode 0 value) <|
        \_ ->
            value
                |> Decode.decodeValue (Decode.succeed ())
                |> Expect.equal (WithWarnings warnings ())


simpleUnexpectedTypes : Test
simpleUnexpectedTypes =
    let
        neutralize : Decoder a -> Decoder ()
        neutralize =
            Decode.map (always ())
    in
    [ ( Encode.string "foo", neutralize Decode.int, "Expected an integer number" )
    , ( Encode.string "foo", neutralize Decode.float, "Expected a number" )
    , ( Encode.string "foo", Decode.null (), "Expected null" )
    , ( Encode.int 12, neutralize Decode.string, "Expected a string" )
    , ( Encode.string "foo", neutralize Decode.bool, "Expected a boolean" )
    , ( Encode.string "foo", neutralize (Decode.list Decode.bool), "Expected an array" )
    , ( Encode.string "bar", Decode.index 0 (Decode.succeed ()), "Expected an array" )
    , ( Encode.list [], Decode.index 0 (Decode.succeed ()), "Expected an array with index 0" )
    , ( Encode.string "foo", neutralize (Decode.keyValuePairs Decode.bool), "Expected an object" )
    , ( Encode.string "bar", Decode.field "foo" (Decode.succeed ()), "Expected an object" )
    , ( Encode.object [], Decode.field "foo" (Decode.succeed ()), "Expected an object with a field 'foo'" )
    ]
        |> List.map (\( val, decoder, expected ) -> simpleUnexpectedType val decoder expected)
        |> describe "Unexpected types, without nesting"


simpleUnexpectedType : Encode.Value -> Decoder () -> String -> Test
simpleUnexpectedType value decoder expected =
    let
        expectedErrors : Errors
        expectedErrors =
            Nonempty
                (Pure <| Failure expected value)
                []
    in
    test ("Given: " ++ Encode.encode 0 value ++ ", expected: " ++ expected) <|
        \_ ->
            value
                |> Decode.decodeValue decoder
                |> Expect.equal (Errors expectedErrors)



-- Simple recursive structure


type Tree
    = Leaf String
    | Branch (List Tree)


treeDecoder : Decoder Tree
treeDecoder =
    Decode.oneOf
        [ Decode.map Leaf Decode.string
        , Decode.map Branch (Decode.list <| Decode.lazy <| \_ -> treeDecoder)
        ]


simpleRecursiveTest : Test
simpleRecursiveTest =
    test "simple recursion" <|
        \_ ->
            """
            [ "1"
            , [ "2", "3"]
            , [ [ "4" ] ]
            ]
        """
                |> Decode.decodeString treeDecoder
                |> Expect.equal
                    (Success
                        (Branch
                            [ Leaf "1"
                            , Branch [ Leaf "2", Leaf "3" ]
                            , Branch [ Branch [ Leaf "4" ] ]
                            ]
                        )
                    )



-- mapX functions


map2Test : Test
map2Test =
    let
        decoder : Decoder Int
        decoder =
            Decode.map2
                (\a b -> a + b)
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
    in
    test "map2" <|
        \_ ->
            """ [ 1, 2 ] """
                |> Decode.decodeString decoder
                |> Expect.equal (Success 3)


map3Test : Test
map3Test =
    let
        decoder : Decoder Int
        decoder =
            Decode.map3
                (\a b c -> a + b + c)
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
                (Decode.index 2 Decode.int)
    in
    test "map3" <|
        \_ ->
            """ [ 1, 2, 3 ] """
                |> Decode.decodeString decoder
                |> Expect.equal (Success 6)


map4Test : Test
map4Test =
    let
        decoder : Decoder Int
        decoder =
            Decode.map4
                (\a b c d -> a + b + c + d)
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
                (Decode.index 2 Decode.int)
                (Decode.index 3 Decode.int)
    in
    test "map4" <|
        \_ ->
            """ [ 1, 2, 3, 4 ] """
                |> Decode.decodeString decoder
                |> Expect.equal (Success 10)


map5Test : Test
map5Test =
    let
        decoder : Decoder Int
        decoder =
            Decode.map5
                (\a b c d e -> a + b + c + d + e)
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
                (Decode.index 2 Decode.int)
                (Decode.index 3 Decode.int)
                (Decode.index 4 Decode.int)
    in
    test "map5" <|
        \_ ->
            """ [ 1, 2, 3, 4, 5 ] """
                |> Decode.decodeString decoder
                |> Expect.equal (Success 15)


map6Test : Test
map6Test =
    let
        decoder : Decoder Int
        decoder =
            Decode.map6
                (\a b c d e f -> a + b + c + d + e + f)
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
                (Decode.index 2 Decode.int)
                (Decode.index 3 Decode.int)
                (Decode.index 4 Decode.int)
                (Decode.index 5 Decode.int)
    in
    test "map6" <|
        \_ ->
            """ [ 1, 2, 3, 4, 5, 6 ] """
                |> Decode.decodeString decoder
                |> Expect.equal (Success 21)


map7Test : Test
map7Test =
    let
        decoder : Decoder Int
        decoder =
            Decode.map7
                (\a b c d e f g -> a + b + c + d + e + f + g)
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
                (Decode.index 2 Decode.int)
                (Decode.index 3 Decode.int)
                (Decode.index 4 Decode.int)
                (Decode.index 5 Decode.int)
                (Decode.index 6 Decode.int)
    in
    test "map7" <|
        \_ ->
            """ [ 1, 2, 3, 4, 5, 6, 7 ] """
                |> Decode.decodeString decoder
                |> Expect.equal (Success 28)


map8Test : Test
map8Test =
    let
        decoder : Decoder Int
        decoder =
            Decode.map8
                (\a b c d e f g h -> a + b + c + d + e + f + g + h)
                (Decode.index 0 Decode.int)
                (Decode.index 1 Decode.int)
                (Decode.index 2 Decode.int)
                (Decode.index 3 Decode.int)
                (Decode.index 4 Decode.int)
                (Decode.index 5 Decode.int)
                (Decode.index 6 Decode.int)
                (Decode.index 7 Decode.int)
    in
    test "map8" <|
        \_ ->
            """ [ 1, 2, 3, 4, 5, 6, 7, 8 ] """
                |> Decode.decodeString decoder
                |> Expect.equal (Success 36)



-- Unused values


noUnusedValuesWithValue : Test
noUnusedValuesWithValue =
    [ Encode.list [ Encode.string "foo" ]
    , Encode.object [ ( "foo", Encode.string "foo" ) ]
    ]
        |> List.map noUnusedValuesWithValueHelper
        |> describe "No unused values in recursive structures when decoding with `value`"


noUnusedValuesWithValueHelper : Encode.Value -> Test
noUnusedValuesWithValueHelper value =
    test ("No unused values for " ++ Encode.encode 0 value) <|
        \_ ->
            value
                |> Decode.decodeValue (Decode.map (always ()) Decode.value)
                |> Expect.equal (Success ())



-- Invalid JSON Values


functionInValueIsBad : Test
functionInValueIsBad =
    test "A function in a JSON value results in BadJson" <|
        \_ ->
            Native.TestHelpers.jsonWithFunction
                |> Decode.decodeValue Decode.value
                |> Expect.equal BadJson



--


listCollectsErrors : Test
listCollectsErrors =
    let
        badIndex : Int -> Located Error
        badIndex idx =
            AtIndex idx <|
                Nonempty
                    (Pure <| Failure "Expected a string" (Encode.int idx))
                    []

        expectedErrors : Errors
        expectedErrors =
            Nonempty (badIndex 0) [ badIndex 1 ]
    in
    test "Multiple errors in lists collect" <|
        \_ ->
            """ [ 0, 1 ] """
                |> Decode.decodeString (Decode.list Decode.string)
                |> Expect.equal (Errors expectedErrors)


indexPropagatesErrors : Test
indexPropagatesErrors =
    let
        badIndex : Int -> Located Error
        badIndex idx =
            AtIndex idx <|
                Nonempty
                    (Pure <| Failure "Expected a string" (Encode.int idx))
                    []

        expectedErrors : Errors
        expectedErrors =
            Nonempty (badIndex 0) []
    in
    test "Errors in indexed decoders propagate" <|
        \_ ->
            """ [ 0, 1 ] """
                |> Decode.decodeString (Decode.index 0 Decode.string)
                |> Expect.equal (Errors expectedErrors)


keyValuePairsCollectsErrors : Test
keyValuePairsCollectsErrors =
    let
        badField : String -> Located Error
        badField field =
            InField field <|
                Nonempty
                    (Pure <| Failure "Expected a string" Encode.null)
                    []

        expectedErrors : Errors
        expectedErrors =
            Nonempty (badField "foo") [ badField "bar" ]
    in
    test "Multiple errors in keyValuePairs collect" <|
        \_ ->
            """ { "foo": null, "hello": "world", "bar": null } """
                |> Decode.decodeString (Decode.keyValuePairs Decode.string)
                |> Expect.equal (Errors expectedErrors)



-- Map2 combines errors


map2CombineErrors : Test
map2CombineErrors =
    let
        badIndex : Int -> Located Error
        badIndex idx =
            AtIndex idx <|
                Nonempty
                    (Pure <| Failure "Expected an integer number" Encode.null)
                    []
    in
    [ ( """ [ null, null ] """, Errors <| Nonempty (badIndex 0) [ badIndex 1 ] )
    , ( """ [ 1, null ] """, Errors <| Nonempty (badIndex 1) [] )
    , ( """ [ null, 1 ] """, Errors <| Nonempty (badIndex 0) [] )
    , ( """ [ 1, 2] """, Success 3 )
    ]
        |> List.map (uncurry map2CombineErrorsCase)
        |> describe "map2 combines errors"


map2CombineErrorsCase : String -> DecodeResult Int -> Test
map2CombineErrorsCase input expected =
    test input <|
        \_ ->
            input
                |> Decode.decodeString
                    (Decode.map2 (+)
                        (Decode.index 0 Decode.int)
                        (Decode.index 1 Decode.int)
                    )
                |> Expect.equal expected


andThenPreservesErrors : Test
andThenPreservesErrors =
    let
        decoder : Decoder ()
        decoder =
            Decode.string
                |> Decode.andThen (\_ -> Decode.succeed ())
                |> Decode.andThen (\_ -> Decode.succeed ())
    in
    test "andThen preserves errors" <|
        \_ ->
            "null"
                |> Decode.decodeString decoder
                |> Expect.equal
                    (Errors <|
                        Nonempty
                            (Pure <| Failure "Expected a string" Encode.null)
                            []
                    )


decodingAFieldDoesNotMarkTheOthersAsUsed : Test
decodingAFieldDoesNotMarkTheOthersAsUsed =
    let
        expectedWarnings : Warnings
        expectedWarnings =
            UnusedValue Encode.null
                |> Pure
                |> Nonempty.fromElement
                |> InField "baz"
                |> Nonempty.fromElement
    in
    test "Decoding a single field generates unused warnings for the other fields" <|
        \_ ->
            """ { "foo": "bar", "baz": null } """
                |> Decode.decodeString (Decode.field "foo" Decode.string)
                |> Expect.equal (WithWarnings expectedWarnings "bar")
