module Json.Decode.Exploration
    exposing
        ( DecodeResult(..)
        , Decoder
        , Error(..)
        , Errors
        , Value
        , Warning(..)
        , Warnings
        , andMap
        , andThen
        , array
        , at
        , bool
        , check
        , decodeString
        , decodeValue
        , dict
        , fail
        , field
        , float
        , index
        , int
        , isArray
        , isObject
        , keyValuePairs
        , lazy
        , list
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , maybe
        , null
        , nullable
        , oneOf
        , string
        , succeed
        , value
        )

{-| Like the regular decoders, except

Examples assume imports:

    import Json.Encode as Encode
    import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))
    import Array
    import Dict


# Run Decoders

@docs decodeString, decodeValue, DecodeResult, Value, Errors, Error, Warnings, Warning


# Primitives

@docs Decoder, string, bool, int, float


# Data Structures

@docs nullable, list, array, dict, keyValuePairs


# Structural ascertainments

@docs isObject, isArray


# Object Primitives

@docs field, at, index


# Inconsistent Structure

@docs maybe, oneOf


# Fancy Decoding

@docs lazy, value, null, check, succeed, fail, andThen


# Mapping

**Note:** If you run out of map functions, take a look at [the pipeline module][pipe]
which makes it easier to handle large objects.

[pipe]: http://package.elm-lang.org/packages/zwilias/json-decode-exploration/latest/Json-Decode-Exploration-Pipeline

@docs map, map2, map3, map4, map5, map6, map7, map8, andMap

Last but not least, an extra import to allow elm-verify-examples to actually
verify all the examples:

    import DocVerificationHelpers exposing (Pet(..))

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))


{-| A simple type alias for `Json.Decode.Value`.
-}
type alias Value =
    Decode.Value


{-| Decoding may fail with 1 or more errors, so `Errors` is a
[`Nonempty`][nonempty] of errors.

[nonempty]: TODO

-}
type alias Errors =
    Nonempty Error


{-| The most basic kind of an `Error` is `Failure`, which comes annotated with
a string describing the failure, and the JSON `Value` that was encountered
instead.

The other cases describe the "path" to where the error occurred.

-}
type Error
    = BadField String Errors
    | BadIndex Int Errors
    | BadOneOf (List Errors)
    | Failure String Value


{-| Decoding may generate warnings. In case the result is a `WithWarnings`, you
will have 1 or more warnings, as a `Nonempty` list.
-}
type alias Warnings =
    Nonempty Warning


{-| Like with errors, the most basic warning is an unused value. The other cases
describe the path to the warnings.
-}
type Warning
    = InField String Warnings
    | AtIndex Int Warnings
    | UnusedValue Value


{-| Decoding can have 4 different outcomes:

  - `BadJson` occurs when the JSON string isn't valid JSON, or the `Value`
    contains non-JSON primitives like functions.
  - `Errors` means errors occurred while running your decoder and contains the
    [`Errors`](#errors) that occurred.
  - `WithWarnings` means decoding succeeded but produced one or more
    [`Warnings`](#warnings).
  - `Success` is the best possible outcome: All went well!

-}
type DecodeResult a
    = BadJson
    | Errors Errors
    | WithWarnings Warnings a
    | Success a


{-| Kind of the core idea of this library. Think of it as a piece of data that
described _how_ to read and transform JSON. You can use `decodeString` and
`decodeValue` to actually execute a decoder on JSON.
-}
type Decoder a
    = Decoder (AnnotatedValue -> Result Errors ( AnnotatedValue, a ))


{-| Run a `Decoder` on a `Value`.

Note that this may still fail with a `BadJson` if there are non-JSON compatible
values in the provided `Value`. In particular, don't attempt to use this library
when decoding `Event`s - it will blow up. Badly.

-}
decodeValue : Decoder a -> Value -> DecodeResult a
decodeValue (Decoder decoderFn) value =
    case decode value of
        Err _ ->
            BadJson

        Ok json ->
            case decoderFn json of
                Err errors ->
                    Errors errors

                Ok ( processedJson, val ) ->
                    case gatherWarnings processedJson of
                        [] ->
                            Success val

                        x :: xs ->
                            WithWarnings (Nonempty x xs) val


{-| Decode a JSON string. If the string isn't valid JSON, this will fail with a
`BadJson` result.
-}
decodeString : Decoder a -> String -> DecodeResult a
decodeString decoder jsonString =
    case Decode.decodeString Decode.value jsonString of
        Err _ ->
            BadJson

        Ok json ->
            decodeValue decoder json


{-| A decoder that will ignore the actual JSON and succeed with the provided
value. Note that this may still fail when dealing with an invalid JSON string.

If a value in the JSON ends up being ignored because of this, this will cause a
warning.

    """ null """
        |> decodeString (value |> andThen (\_ -> succeed "hello world"))
    --> Success "hello world"


    """ null """
        |> decodeString (succeed "hello world")
    --> WithWarnings
    -->     (Nonempty (UnusedValue Encode.null) [])
    -->     "hello world"


    """ foo """
        |> decodeString (succeed "hello world")
    --> BadJson

-}
succeed : a -> Decoder a
succeed val =
    Decoder <| \json -> Ok ( json, val )


{-| Ignore the json and fail with a provided message.

    """ "hello" """
        |> decodeString (fail "failure")
    --> Errors (Nonempty (Failure "failure" (Encode.string "hello")) [])

-}
fail : String -> Decoder a
fail message =
    Decoder <|
        \json ->
            encode json
                |> Failure message
                |> Nonempty.fromElement
                |> Err


{-| Decode a string.

    """ "hello world" """
        |> decodeString string
    --> Success "hello world"


    """ 123 """
        |> decodeString string
    --> Errors (Nonempty (Failure "Expected a string" (Encode.int 123)) [])

-}
string : Decoder String
string =
    Decoder <|
        \json ->
            case json of
                String _ value ->
                    Ok ( markUsed json, value )

                _ ->
                    expected "a string" json


{-| Extract a piece without actually decoding it.

If a structure is decoded as a `value`, everything _in_ the structure will be
considered as having been used and will not appear in `UnusedValue` warnings.

    """ [ 123, "world" ] """
        |> decodeString value
    --> Success (Encode.list [ Encode.int 123, Encode.string "world" ])

-}
value : Decoder Value
value =
    Decoder <|
        \json ->
            Ok ( markUsed json, encode json )


{-| Decode a number into a `Float`.

    """ 12.34 """
        |> decodeString float
    --> Success 12.34


    """ 12 """
        |> decodeString float
    --> Success 12


    """ null """
        |> decodeString float
    --> Errors (Nonempty (Failure "Expected a number" Encode.null) [])

-}
float : Decoder Float
float =
    Decoder <|
        \json ->
            case json of
                Number _ value ->
                    Ok ( markUsed json, value )

                _ ->
                    expected "a number" json


{-| Decode a number into an `Int`.

    """ 123 """
        |> decodeString int
    --> Success 123


    """ 0.1 """
        |> decodeString int
    --> Errors <|
    -->   Nonempty
    -->     (Failure "Expected an integer number" (Encode.float 0.1))
    -->     []

-}
int : Decoder Int
int =
    Decoder <|
        \json ->
            case json of
                Number _ value ->
                    if toFloat (round value) == value then
                        Ok ( markUsed json, round value )
                    else
                        expected "an integer number" json

                _ ->
                    expected "an integer number" json


{-| Decode a boolean value.

    """ [ true, false ] """
        |> decodeString (list bool)
    --> Success [ True, False ]

-}
bool : Decoder Bool
bool =
    Decoder <|
        \json ->
            case json of
                Bool _ value ->
                    Ok ( markUsed json, value )

                _ ->
                    expected "a boolean" json


{-| Decode a `null` and succeed with some value.

    """ null """
        |> decodeString (null "it was null")
    --> Success "it was null"

Note that `undefined` and `null` are not the same thing. This cannot be used to
verify that a field is _missing_, only that it is explicitly set to `null`.

    """ { "foo": null } """
        |> decodeString (field "foo" (null ()))
    --> Success ()


    """ { } """
        |> decodeString (field "foo" (null ()))
    --> Errors <|
    -->   Nonempty
    -->     (Failure "Expected an object with a field 'foo'" (Encode.object []))
    -->     []

-}
null : a -> Decoder a
null val =
    Decoder <|
        \json ->
            case json of
                Null _ ->
                    Ok ( Null True, val )

                _ ->
                    expected "null" json


{-| Decode a list of values, decoding each entry with the provided decoder.

    """ [ "foo", "bar" ] """
        |> decodeString (list string)
    --> Success [ "foo", "bar" ]


    """ [ "foo", null ] """
        |> decodeString (list string)
    --> Errors <|
    -->   Nonempty
    -->     (BadIndex 1 <|
    -->       Nonempty (Failure "Expected a string" Encode.null) []
    -->     )
    -->     []

-}
list : Decoder a -> Decoder (List a)
list (Decoder decoderFn) =
    let
        accumulate :
            AnnotatedValue
            -> ( Int, Result Errors ( List AnnotatedValue, List a ) )
            -> ( Int, Result Errors ( List AnnotatedValue, List a ) )
        accumulate value ( idx, acc ) =
            case ( acc, decoderFn value ) of
                ( Err errors, Err newErrors ) ->
                    ( idx - 1
                    , Err <| Nonempty.cons (BadIndex idx newErrors) errors
                    )

                ( Err errors, _ ) ->
                    ( idx - 1, Err errors )

                ( _, Err errors ) ->
                    ( idx - 1
                    , Err <| Nonempty.fromElement (BadIndex idx errors)
                    )

                ( Ok ( jsonAcc, valAcc ), Ok ( json, val ) ) ->
                    ( idx - 1, Ok ( json :: jsonAcc, val :: valAcc ) )
    in
    Decoder <|
        \json ->
            case json of
                Array _ values ->
                    List.foldr accumulate
                        ( List.length values - 1, Ok ( [], [] ) )
                        values
                        |> Tuple.second
                        |> Result.map (Tuple.mapFirst (Array True))

                _ ->
                    expected "an array" json


{-| _Convenience function._ Decode a JSON array into an Elm `Array`.

    """ [ 1, 2, 3 ] """
        |> decodeString (array int)
    --> Success <| Array.fromList [ 1, 2, 3 ]

-}
array : Decoder a -> Decoder (Array a)
array decoder =
    map Array.fromList (list decoder)


{-| _Convenience function._ Decode a JSON object into an Elm `Dict String`.

    """ { "foo": "bar", "bar": "hi there" } """
        |> decodeString (dict string)
    --> Success <| Dict.fromList
    -->   [ ( "bar", "hi there" )
    -->   , ( "foo", "bar" )
    -->   ]

-}
dict : Decoder v -> Decoder (Dict String v)
dict decoder =
    map Dict.fromList (keyValuePairs decoder)


{-| A Decoder to ascertain that a JSON value _is_ in fact, a JSON object.

Using this decoder marks the object itself as used, without touching any of its
children. It is, as such, fairly well behaved.

    """ { } """
        |> decodeString isObject
    --> Success ()


    """ [] """
        |> decodeString isObject
    --> Errors <| Nonempty.fromElement <| Failure "Expected an object" (Encode.list [])

-}
isObject : Decoder ()
isObject =
    Decoder <|
        \json ->
            case json of
                Object _ pairs ->
                    Ok ( Object True pairs, () )

                _ ->
                    expected "an object" json


{-| Similar to `isObject`, a decoder to ascertain that a JSON value is a JSON
array.

    """ [] """
        |> decodeString isArray
    --> Success ()


    """ [ "foo" ] """
        |> decodeString isArray
    --> WithWarnings (Nonempty (AtIndex 0 (Nonempty (UnusedValue <|
            Encode.string "foo") [])) []) ()

-}
isArray : Decoder ()
isArray =
    Decoder <|
        \json ->
            case json of
                Array _ values ->
                    Ok ( Array True values, () )

                _ ->
                    expected "an array" json


{-| Decode a specific index using a specified `Decoder`.

    """ [ "hello", 123 ] """
        |> decodeString (map2 (,) (index 0 string) (index 1 int))
    --> Success ( "hello", 123 )


    """ [ "hello", "there" ] """
        |> decodeString (index 1 string)
    --> WithWarnings
    -->   (Nonempty (AtIndex 0 <| Nonempty (UnusedValue (Encode.string "hello")) []) [])
    -->   "there"

-}
index : Int -> Decoder a -> Decoder a
index idx (Decoder decoderFn) =
    let
        finalize :
            AnnotatedValue
            -> ( b, List AnnotatedValue, Maybe (Result Errors a) )
            -> Result Errors ( AnnotatedValue, a )
        finalize json ( _, values, res ) =
            case res of
                Nothing ->
                    expected ("an array with index " ++ toString idx) json

                Just (Err e) ->
                    Err e

                Just (Ok v) ->
                    Ok ( Array True values, v )

        accumulate :
            AnnotatedValue
            -> ( Int, List AnnotatedValue, Maybe (Result Errors a) )
            -> ( Int, List AnnotatedValue, Maybe (Result Errors a) )
        accumulate value ( i, acc, result ) =
            if i == idx then
                case decoderFn value of
                    Err e ->
                        ( i - 1
                        , value :: acc
                        , Just <| Err <| Nonempty.fromElement <| BadIndex i e
                        )

                    Ok ( updatedJson, res ) ->
                        ( i - 1
                        , updatedJson :: acc
                        , Just <| Ok res
                        )
            else
                ( i - 1
                , value :: acc
                , result
                )
    in
    Decoder <|
        \json ->
            case json of
                Array _ values ->
                    List.foldr
                        accumulate
                        ( List.length values - 1, [], Nothing )
                        values
                        |> finalize json

                _ ->
                    expected "an array" json


{-| Decode a JSON object into a list of key-value pairs. The decoder you provide
will be used to decode the values.

    """ { "foo": "bar", "hello": "world" } """
        |> decodeString (keyValuePairs string)
    --> Success [ ( "foo", "bar" ), ( "hello", "world" ) ]

-}
keyValuePairs : Decoder a -> Decoder (List ( String, a ))
keyValuePairs (Decoder decoderFn) =
    let
        accumulate :
            ( String, AnnotatedValue )
            -> Result Errors ( List ( String, AnnotatedValue ), List ( String, a ) )
            -> Result Errors ( List ( String, AnnotatedValue ), List ( String, a ) )
        accumulate ( key, value ) acc =
            case ( acc, decoderFn value ) of
                ( Err e, Err new ) ->
                    Err <| Nonempty.cons (BadField key new) e

                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err <| Nonempty.fromElement (BadField key e)

                ( Ok ( jsonAcc, resAcc ), Ok ( newJson, newRes ) ) ->
                    Ok
                        ( ( key, newJson ) :: jsonAcc
                        , ( key, newRes ) :: resAcc
                        )
    in
    Decoder <|
        \json ->
            case json of
                Object _ kvPairs ->
                    List.foldr accumulate (Ok ( [], [] )) kvPairs
                        |> Result.map (Tuple.mapFirst (Object True))

                _ ->
                    expected "an object" json


{-| Decode the content of a field using a provided decoder.

    """ { "foo": "bar" } """
        |> decodeString (field "foo" string)
    --> Success "bar"


    """ [ { "foo": "bar" }, { "foo": "baz", "hello": "world" } ] """
        |> decodeString (list (field "foo" string))
    --> WithWarnings expectedWarnings [ "bar", "baz" ]


    expectedWarnings : Warnings
    expectedWarnings =
        UnusedValue (Encode.string "world")
            |> Nonempty.fromElement
            |> InField "hello"
            |> Nonempty.fromElement
            |> AtIndex 1
            |> Nonempty.fromElement

-}
field : String -> Decoder a -> Decoder a
field fieldName (Decoder decoderFn) =
    let
        accumulate :
            ( String, AnnotatedValue )
            -> ( List ( String, AnnotatedValue ), Maybe (Result Errors a) )
            -> ( List ( String, AnnotatedValue ), Maybe (Result Errors a) )
        accumulate ( key, value ) ( acc, result ) =
            if key == fieldName then
                case decoderFn value of
                    Err e ->
                        ( ( key, value ) :: acc
                        , Just <| Err <| Nonempty.fromElement <| BadField key e
                        )

                    Ok ( newValue, v ) ->
                        ( ( key, newValue ) :: acc
                        , Just <| Ok v
                        )
            else
                ( ( key, value ) :: acc, result )

        finalize :
            AnnotatedValue
            -> ( List ( String, AnnotatedValue ), Maybe (Result Errors a) )
            -> Result Errors ( AnnotatedValue, a )
        finalize json ( values, res ) =
            case res of
                Nothing ->
                    expected ("an object with a field '" ++ fieldName ++ "'") json

                Just (Err e) ->
                    Err e

                Just (Ok v) ->
                    Ok ( Object True values, v )
    in
    Decoder <|
        \json ->
            case json of
                Object _ kvPairs ->
                    List.foldr accumulate ( [], Nothing ) kvPairs
                        |> finalize json

                _ ->
                    expected "an object" json


{-| Decodes a value at a certain path, using a provided decoder. Essentially,
writing `at [ "a", "b", "c" ]  string` is sugar over writing
`field "a" (field "b" (field "c" string))`}.

    """ { "a": { "b": { "c": "hi there" } } } """
        |> decodeString (at [ "a", "b", "c" ] string)
    --> Success "hi there"

-}
at : List String -> Decoder a -> Decoder a
at fields decoder =
    List.foldr field decoder fields



-- Choosing


{-| Tries a bunch of decoders. The first one to not fail will be the one used.

If all fail, the errors are collected into a `BadOneOf`.

    """ [ 12, "whatever" ] """
        |> decodeString (list <| oneOf [ map toString int, string ])
    --> Success [ "12", "whatever" ]


    """ null """
        |> decodeString (oneOf [ string, map toString int ])
    --> Errors <| Nonempty.fromElement <| BadOneOf
    -->   [ Nonempty.fromElement <| Failure "Expected a string" Encode.null
    -->   , Nonempty.fromElement <| Failure "Expected an integer number" Encode.null
    -->   ]

-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder <|
        \json ->
            oneOfHelp decoders json []


oneOfHelp :
    List (Decoder a)
    -> AnnotatedValue
    -> List Errors
    -> Result Errors ( AnnotatedValue, a )
oneOfHelp decoders value errorAcc =
    case decoders of
        [] ->
            Err <| Nonempty.fromElement <| BadOneOf (List.reverse errorAcc)

        (Decoder decoderFn) :: rest ->
            case decoderFn value of
                Ok ( newJson, res ) ->
                    Ok ( newJson, res )

                Err e ->
                    oneOfHelp rest value (e :: errorAcc)


{-| Decodes successfully and wraps with a `Just`, handling failure by succeeding
with `Nothing`.

    """ [ "foo", 12 ] """
        |> decodeString (list <| maybe string)
    --> WithWarnings expectedWarnings [ Just "foo", Nothing ]


    expectedWarnings : Warnings
    expectedWarnings =
        UnusedValue (Encode.int 12)
            |> Nonempty.fromElement
            |> AtIndex 1
            |> Nonempty.fromElement

-}
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    oneOf [ map Just decoder, succeed Nothing ]


{-| Decodes successfully and wraps with a `Just`. If the values is `null`
succeeds with `Nothing`.

    """ [ { "foo": "bar" }, { "foo": null } ] """
        |> decodeString (list <| field "foo" <| nullable string)
    --> Success [ Just "bar", Nothing ]

-}
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
    oneOf [ null Nothing, map Just decoder ]



--


{-| Required when using (mutually) recursive decoders.
-}
lazy : (() -> Decoder a) -> Decoder a
lazy toDecoder =
    Decoder <|
        \json ->
            let
                (Decoder decoderFn) =
                    toDecoder ()
            in
            decoderFn json



-- Extras


{-| Useful for checking a value in the JSON matches the value you expect it to
have. If it does, succeeds with the second decoder. If it doesn't it fails.

This can be used to decode union types:

    type Pet = Cat | Dog | Rabbit

    petDecoder : Decoder Pet
    petDecoder =
        oneOf
            [ check string "cat" <| succeed Cat
            , check string "dog" <| succeed Dog
            , check string "rabbit" <| succeed Rabbit
            ]

    """ [ "dog", "rabbit", "cat" ] """
        |> decodeString (list petDecoder)
    --> Success [ Dog, Rabbit, Cat ]

-}
check : Decoder a -> a -> Decoder b -> Decoder b
check checkDecoder expected actualDecoder =
    checkDecoder
        |> andThen
            (\actual ->
                if actual == expected then
                    actualDecoder
                else
                    fail <|
                        "Verification failed, expected '"
                            ++ toString expected
                            ++ "'."
            )



-- Mapping and chaining


{-| Useful for transforming decoders.

    """ "foo" """
        |> decodeString (map String.toUpper string)
    --> Success "FOO"

-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoderFn) =
    Decoder <|
        \json ->
            Result.map (Tuple.mapSecond f) (decoderFn json)


{-| Chain decoders where one decoder depends on the value of another decoder.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen toDecoderB (Decoder decoderFnA) =
    Decoder <|
        \json ->
            case decoderFnA json of
                Ok ( newJson, valA ) ->
                    let
                        (Decoder decoderFnB) =
                            toDecoderB valA
                    in
                    decoderFnB newJson

                Err e ->
                    Err e


{-| Combine 2 decoders.
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder decoderFnA) (Decoder decoderFnB) =
    Decoder <|
        \json ->
            case decoderFnA json of
                Ok ( newJson, valA ) ->
                    case decoderFnB newJson of
                        Ok ( finalJson, valB ) ->
                            Ok ( finalJson, f valA valB )

                        Err e ->
                            Err e

                Err e ->
                    case decoderFnB json of
                        Ok _ ->
                            Err e

                        Err e2 ->
                            Err <| Nonempty.append e e2


{-| Decode an argument and provide it to a function in a decoder.

    decoder : Decoder String
    decoder =
        succeed (String.repeat)
            |> andMap (field "count" int)
            |> andMap (field "val" string)


    """ { "val": "hi", "count": 3 } """
        |> decodeString decoder
    --> Success "hihihi"

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


{-| Combine 3 decoders.
-}
map3 :
    (a -> b -> c -> d)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
map3 f decoderA decoderB decoderC =
    map f decoderA
        |> andMap decoderB
        |> andMap decoderC


{-| Combine 4 decoders.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
map4 f decoderA decoderB decoderC decoderD =
    map f decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD


{-| Combine 5 decoders.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
map5 f decoderA decoderB decoderC decoderD decoderE =
    map f decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD
        |> andMap decoderE


{-| Combine 6 decoders.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
map6 f decoderA decoderB decoderC decoderD decoderE decoderF =
    map f decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD
        |> andMap decoderE
        |> andMap decoderF


{-| Combine 7 decoders.
-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
map7 f decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    map f decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD
        |> andMap decoderE
        |> andMap decoderF
        |> andMap decoderG


{-| Combine 8 decoders.
-}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
map8 f decoderA decoderB decoderC decoderD decoderE decoderF decoderG decoderH =
    map f decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD
        |> andMap decoderE
        |> andMap decoderF
        |> andMap decoderG
        |> andMap decoderH



-- Internal stuff


type AnnotatedValue
    = String Bool String
    | Number Bool Float
    | Bool Bool Bool
    | Null Bool
    | Array Bool (List AnnotatedValue)
    | Object Bool (List ( String, AnnotatedValue ))


expected : String -> AnnotatedValue -> Result Errors a
expected expectedType json =
    encode json
        |> Failure ("Expected " ++ expectedType)
        |> Nonempty.fromElement
        |> Err


decode : Value -> Result String AnnotatedValue
decode =
    Decode.decodeValue decoder


decoder : Decode.Decoder AnnotatedValue
decoder =
    Decode.oneOf
        [ Decode.map (String False) Decode.string
        , Decode.map (Number False) Decode.float
        , Decode.map (Bool False) Decode.bool
        , Decode.null (Null False)
        , Decode.map (Array False) (Decode.list <| Decode.lazy <| \_ -> decoder)
        , Decode.map
            (List.reverse >> Object False)
            (Decode.keyValuePairs <| Decode.lazy <| \_ -> decoder)
        ]


encode : AnnotatedValue -> Value
encode v =
    case v of
        String _ value ->
            Encode.string value

        Number _ value ->
            Encode.float value

        Bool _ value ->
            Encode.bool value

        Null _ ->
            Encode.null

        Array _ values ->
            List.map encode values
                |> Encode.list

        Object _ kvPairs ->
            List.map (Tuple.mapSecond encode) kvPairs
                |> Encode.object


gatherWarnings : AnnotatedValue -> List Warning
gatherWarnings json =
    case json of
        String False _ ->
            [ UnusedValue <| encode json ]

        Number False _ ->
            [ UnusedValue <| encode json ]

        Bool False _ ->
            [ UnusedValue <| encode json ]

        Null False ->
            [ UnusedValue <| encode json ]

        Array False _ ->
            [ UnusedValue <| encode json ]

        Object False _ ->
            [ UnusedValue <| encode json ]

        Array _ values ->
            values
                |> List.indexedMap
                    (\idx value ->
                        case gatherWarnings value of
                            [] ->
                                []

                            x :: xs ->
                                [ AtIndex idx <| Nonempty x xs ]
                    )
                |> List.concat

        Object _ kvPairs ->
            kvPairs
                |> List.concatMap
                    (\( key, value ) ->
                        case gatherWarnings value of
                            [] ->
                                []

                            x :: xs ->
                                [ InField key <| Nonempty x xs ]
                    )

        _ ->
            []


markUsed : AnnotatedValue -> AnnotatedValue
markUsed annotatedValue =
    case annotatedValue of
        String _ value ->
            String True value

        Number _ value ->
            Number True value

        Bool _ value ->
            Bool True value

        Null _ ->
            Null True

        Array _ values ->
            Array True (List.map markUsed values)

        Object _ values ->
            Object True (List.map (Tuple.mapSecond markUsed) values)
