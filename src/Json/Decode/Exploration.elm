module Json.Decode.Exploration
    exposing
        ( Decoder
        , Error(..)
        , ErrorType(..)
        , MisMatch(..)
        , Node(..)
        , Path
        , StructuralIssue(..)
        , Value
        , andMap
        , andThen
        , array
        , at
        , bool
        , custom
        , decodeString
        , decodeValue
        , dict
        , fail
        , field
        , float
        , index
        , int
        , keyValuePairs
        , lazy
        , list
        , map
        , map2
        , map3
        , map4
        , map5
        , maybe
        , null
        , nullable
        , oneOf
        , string
        , succeed
        , toClassic
        , value
        )

{-| An experiment with adding stronger error-types to JSON decoders.


# Primitives

@docs Decoder, string, bool, int, float


# Data Structures

@docs nullable, list, array, dict, keyValuePairs


# Object Primitives

@docs field, at, index


# Inconsistent Structure

@docs maybe, oneOf


# Run Decoders

@docs decodeString, decodeValue, Value, Path, Node


# Errors

@docs Error, ErrorType, MisMatch, StructuralIssue


# Interop

@docs custom, toClassic


# Mapping

@docs andMap, map, map2, map3, map4, map5


# Fancy Decoding

@docs lazy, value, null, succeed, fail, andThen

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as JD


{-| A JSON value.
-}
type alias Value =
    JD.Value


{-| A decode error.
-}
type Error
    = InvalidJSON
    | Error ErrorType
    | InContext { path : Path, error : ErrorType }


{-| A specific type of error.
-}
type ErrorType
    = Structural StructuralIssue
    | TypeMisMatch MisMatch
    | BadOneOf (List Error)
    | BadMap Error Error
    | Custom String


{-| A type mistmatch.
-}
type MisMatch
    = ExpectedInt
    | ExpectedFloat
    | ExpectedString
    | ExpectedNull
    | ExpectedBool


{-| A structural issue with your JSON.
-}
type StructuralIssue
    = ExpectedField String
    | ExpectedArrayIndex Int
    | ExpectedArray
    | ExpectedObject


{-| A path through your JSON that leads to the problematic value.
-}
type alias Path =
    List Node


{-| A piece of the path, either a field or an index in an array.
-}
type Node
    = Field String
    | Index Int


{-| A Decoder is simply a function that turns a `Value` into a `Result Error a`
-}
type alias Decoder a =
    Value -> Result Error a


{-| Parse the given string into a JSON value and then run the `Decoder` on it.
This will fail if the string is not well-formed JSON or if the `Decoder` fails
for some reason.
-}
decodeString : Decoder a -> String -> Result Error a
decodeString decoder input =
    case JD.decodeString JD.value input of
        Ok v ->
            decodeValue decoder v

        Err _ ->
            Err InvalidJSON


{-| Run a `Decoder` on some JSON `Value`. You can send these JSON values through
ports, so that is probably the main time you would use this function.
-}
decodeValue : Decoder a -> Value -> Result Error a
decodeValue decoder input =
    decoder input


{-| Decode a JSON number into an Elm `Int`.
-}
int : Decoder Int
int =
    wrapPrimitive JD.int (TypeMisMatch ExpectedInt)


inPath : Node -> Result Error a -> Result Error a
inPath path =
    Result.mapError
        (\error ->
            case error of
                InvalidJSON ->
                    InvalidJSON

                Error errorType ->
                    InContext { path = [ path ], error = errorType }

                InContext err ->
                    InContext { err | path = path :: err.path }
        )


{-| Doesn't modify the JS value, just returns it.
-}
value : Decoder Value
value =
    Ok


{-| Allow using a classical `Json.Decode.Decoder` decoder.
-}
custom : JD.Decoder a -> Decoder a
custom decoder =
    JD.decodeValue decoder
        >> Result.mapError (Custom >> Error)


{-| Allow using an exploration decoder in a regular `Json.Decode` context.
-}
toClassic : Decoder a -> JD.Decoder a
toClassic decoder =
    JD.value
        |> JD.andThen
            (\input ->
                case decoder input of
                    Ok v ->
                        JD.succeed v

                    Err err ->
                        JD.fail (toString err)
            )


{-| Decodes the contents of the field named in the first argument with the
passed decoder.
-}
field : String -> Decoder a -> Decoder a
field fieldName decoder =
    \input ->
        case JD.decodeValue (JD.field fieldName JD.value) input of
            Ok fieldVal ->
                decoder fieldVal
                    |> inPath (Field fieldName)

            Err _ ->
                Err (Error (Structural <| ExpectedField fieldName))


{-| Decodes the contents of the field named by following the path constructed by
the fieldnames passed as the first arg with the given decoder.
-}
at : List String -> Decoder a -> Decoder a
at path decoder =
    List.foldr field decoder path


wrapPrimitive : JD.Decoder a -> ErrorType -> Decoder a
wrapPrimitive primitive error =
    JD.decodeValue primitive
        >> Result.mapError (always error >> Error)


{-| Transform a decoder by mapping the result.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f decoder =
    decoder >> Result.map f


{-| Executes a second decoder with the result of another decoder as its first
argument.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen toDecoder decoder val =
    decoder val |> Result.andThen (\output -> toDecoder output val)


{-| Decodes a JSON object into a list of key-value pairs.
-}
keyValuePairs : Decoder a -> Decoder (List ( String, a ))
keyValuePairs decoder input =
    case JD.decodeValue (JD.keyValuePairs JD.value) input of
        Ok kvs ->
            List.foldl
                (\( key, val ) vals ->
                    decoder val
                        |> inPath (Field key)
                        |> Result.map2 (\xs x -> ( key, x ) :: xs) vals
                )
                (Ok [])
                kvs

        Err _ ->
            ExpectedObject |> Structural |> Error |> Err


{-| Decodes a JSON array into an Elm `List`.
-}
list : Decoder a -> Decoder (List a)
list decoder input =
    case JD.decodeValue (JD.list JD.value) input of
        Ok vals ->
            List.foldl
                (\val ( r, idx ) ->
                    decoder val
                        |> inPath (Index idx)
                        |> Result.map2 (flip (::)) r
                        |> flip (,) (idx + 1)
                )
                ( Ok [], 0 )
                vals
                |> Tuple.first
                |> Result.map List.reverse

        Err _ ->
            ExpectedArray |> Structural |> Error |> Err


{-| Decode a JSON null into an Elm value.
-}
null : a -> Decoder a
null v =
    wrapPrimitive (JD.null v) (TypeMisMatch ExpectedNull)


{-| Decode a JSON string into an Elm `String`.
-}
string : Decoder String
string =
    wrapPrimitive JD.string (TypeMisMatch ExpectedString)


{-| A decoder that always fails.
-}
fail : String -> Decoder a
fail msg input =
    Err <| Error <| Custom msg


{-| Tries these decoders one by one. The first one to succeed will have its
value returned. If no decoders were passed or all decoders fail, you'll get an
error.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders input =
    let
        tryDecoder : Decoder a -> Result (List Error) a -> Result (List Error) a
        tryDecoder decoder result =
            case result of
                Err errs ->
                    case decoder input of
                        Ok v ->
                            Ok v

                        Err err ->
                            Err (err :: errs)

                Ok _ ->
                    result
    in
    List.foldl tryDecoder (Err []) decoders
        |> Result.mapError (List.reverse >> BadOneOf >> Error)


{-| A decoder that always succeeds.
-}
succeed : a -> Decoder a
succeed val _ =
    Ok val


{-| Decode a JSON boolean into an Elm `Bool`.
-}
bool : Decoder Bool
bool =
    wrapPrimitive JD.bool (TypeMisMatch ExpectedBool)


{-| Decode a JSON number into an Elm `Float`.
-}
float : Decoder Float
float =
    wrapPrimitive JD.float (TypeMisMatch ExpectedFloat)


{-| Decode a value that must exist but _could_ be `null`.
-}
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
    oneOf
        [ map Just decoder
        , null Nothing
        ]


{-| Decode a JSON array into an Elm `Array`.
-}
array : Decoder a -> Decoder (Array a)
array =
    list >> map Array.fromList


{-| Decode a JSON object into an Elm `Dict`.
-}
dict : Decoder a -> Decoder (Dict String a)
dict =
    keyValuePairs >> map Dict.fromList


{-| Decode the value at a given index in a JSON array.
-}
index : Int -> Decoder a -> Decoder a
index idx decoder input =
    case JD.decodeValue (JD.index idx JD.value) input of
        Ok atIndex ->
            decoder input
                |> inPath (Index idx)

        Err _ ->
            Err <| Error <| Structural <| ExpectedArrayIndex idx


{-| Decode a value that may or may not exist, or might be malformed.
-}
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    oneOf
        [ map Just decoder
        , succeed Nothing
        ]


{-| Execute a function on the result of 2 decoders.
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f aDecoder bDecoder input =
    case ( aDecoder input, bDecoder input ) of
        ( Ok a, Ok b ) ->
            Ok <| f a b

        ( Err a, Err b ) ->
            Err <| Error <| BadMap b a

        ( Err a, _ ) ->
            Err a

        ( _, Err b ) ->
            Err b


{-| Execute a function on the result of 3 decoders.
-}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f aDecoder bDecoder cDecoder =
    succeed f
        |> andMap aDecoder
        |> andMap bDecoder
        |> andMap cDecoder


{-| Execute a function on the result of 4 decoders.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
map4 f aDecoder bDecoder cDecoder dDecoder =
    succeed f
        |> andMap aDecoder
        |> andMap bDecoder
        |> andMap cDecoder
        |> andMap dDecoder


{-| Execute a function on the result of 5 decoders.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
map5 f aDecoder bDecoder cDecoder dDecoder eDecoder =
    succeed f
        |> andMap aDecoder
        |> andMap bDecoder
        |> andMap cDecoder
        |> andMap dDecoder
        |> andMap eDecoder


{-| Allows pipelining decoders rather than having large `mapN` calls.
-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


{-| Creates a lazy decoder, useful for recursive structures.
-}
lazy : (() -> Decoder a) -> Decoder a
lazy decoder input =
    decoder () input
