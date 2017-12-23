module PipelineTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode.Exploration as Decode exposing (..)
import Json.Decode.Exploration.Located exposing (Located(..))
import Json.Decode.Exploration.Pipeline exposing (..)
import Json.Encode as Encode
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Test exposing (..)


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


optionalEmptyStructure : Test
optionalEmptyStructure =
    test "decoding an optional field in an empty object should mark the object as used" <|
        \_ ->
            """ {} """
                |> decodeString (decode identity |> optional "foo" string "hi")
                |> Expect.equal (Success "hi")


optionalUnusedField : Test
optionalUnusedField =
    let
        expectedWarnings : Warnings
        expectedWarnings =
            UnusedValue (Encode.int 1)
                |> Pure
                |> Nonempty.fromElement
                |> InField "a"
                |> Nonempty.fromElement
    in
    test "decoding an optional field in an object with one other field should warn about other field" <|
        \_ ->
            """ { "a": 1 } """
                |> decodeString (decode identity |> optional "b" string "hi")
                |> Expect.equal (WithWarnings expectedWarnings "hi")


optionalWrongStructure : Test
optionalWrongStructure =
    test "Decoding an optional field fails if the item is not, in fact, an object" <|
        \_ ->
            """ [] """
                |> decodeString (decode identity |> optional "foo" string "hi")
                |> Expect.equal (Errors (Nonempty (Pure <| Expected TObject (Encode.list [])) []))


optionalAtWrongStructure : Test
optionalAtWrongStructure =
    let
        expectedErrors : Errors
        expectedErrors =
            Expected TObject (Encode.list [])
                |> Pure
                |> Nonempty.fromElement
                |> InField "b"
                |> Nonempty.fromElement
                |> InField "a"
                |> Nonempty.fromElement
    in
    test "Using optionalAt where a field on the path is of an unexpected type fails" <|
        \_ ->
            """ { "a": { "b": [] } } """
                |> decodeString (decode identity |> optionalAt [ "a", "b", "c" ] string "hi")
                |> Expect.equal (Errors expectedErrors)
