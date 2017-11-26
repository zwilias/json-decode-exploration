module SimpleTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode.Exploration as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Nonempty exposing (Nonempty(..))
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
        warnings : Decode.Warnings
        warnings =
            Nonempty (Decode.UnusedValue value) []
    in
    test (Encode.encode 0 value) <|
        \_ ->
            value
                |> Decode.decodeValue (Decode.succeed ())
                |> Expect.equal (Decode.WithWarnings warnings ())


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
    , ( Encode.string "foo", neutralize (Decode.keyValuePairs Decode.bool), "Expected an object" )
    , ( Encode.list [], Decode.index 0 (Decode.succeed ()), "Expected an array with index 0" )
    , ( Encode.object [], Decode.field "foo" (Decode.succeed ()), "Expected an object with a field 'foo'" )
    ]
        |> List.map (\( val, decoder, expected ) -> simpleUnexpectedType val decoder expected)
        |> describe "Unexpected types, without nesting"


simpleUnexpectedType : Encode.Value -> Decoder () -> String -> Test
simpleUnexpectedType value decoder expected =
    let
        expectedErrors : Decode.Errors
        expectedErrors =
            Nonempty
                (Decode.Failure expected value)
                []
    in
    test expected <|
        \_ ->
            value
                |> Decode.decodeValue decoder
                |> Expect.equal (Decode.Errors expectedErrors)



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
                    (Decode.Success
                        (Branch
                            [ Leaf "1"
                            , Branch [ Leaf "2", Leaf "3" ]
                            , Branch [ Branch [ Leaf "4" ] ]
                            ]
                        )
                    )
