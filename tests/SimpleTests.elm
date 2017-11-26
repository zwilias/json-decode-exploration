module SimpleTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode.Exploration as Decode exposing (..)
import Json.Decode.Exploration.Pipeline exposing (..)
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
        warnings : Nonempty Warning
        warnings =
            Nonempty (UnusedValue value) []
    in
    test (Encode.encode 0 value) <|
        \_ ->
            value
                |> decodeValue (Decode.succeed ())
                |> Expect.equal (WithWarnings warnings ())


simpleUnexpectedTypes : Test
simpleUnexpectedTypes =
    let
        neutralize : Decoder a -> Decoder ()
        neutralize =
            map (always ())
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
        expectedErrors : Errors
        expectedErrors =
            Nonempty
                (Failure expected value)
                []
    in
    test expected <|
        \_ ->
            value
                |> decodeValue decoder
                |> Expect.equal (Errors expectedErrors)


optionalAtTest : Test
optionalAtTest =
    let
        decoder : Decoder (Maybe (List Int))
        decoder =
            decode identity
                |> optionalAt [ "a", "b", "c" ] (Decode.list Decode.int |> Decode.map Just) Nothing
    in
    describe "Json.Decode.Exploration.Pipeline"
        [ test "should work with optionalAt" <|
            \() ->
                """ {"a": {"b": {"c": [1,2,3]}}} """
                    |> decodeString decoder
                    |> Expect.equal (Success <| Just [ 1, 2, 3 ])
        ]
