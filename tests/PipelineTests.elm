module PipelineTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode.Exploration as Decode exposing (..)
import Json.Decode.Exploration.Pipeline exposing (..)
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
