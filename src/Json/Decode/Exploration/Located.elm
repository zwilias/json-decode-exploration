module Json.Decode.Exploration.Located exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(..))
import Set exposing (Set)


type Located a
    = InField String (Nonempty (Located a))
    | AtIndex Int (Nonempty (Located a))
    | Pure a


toString : (a -> List String) -> Nonempty (Located a) -> List String
toString itemToString locatedItems =
    locatedItems
        |> gather ""
        |> List.map (uncurry <| render itemToString)
        |> intercalate "And also"


intercalate : a -> List (List a) -> List a
intercalate sep lists =
    lists |> List.intersperse [ sep ] |> List.concat


render : (a -> List String) -> String -> List a -> List String
render itemToString path errors =
    List.concatMap itemToString errors
        |> List.map indent
        |> (::) ("At path " ++ path)


indent : String -> String
indent =
    (++) "  "


flatten : Located a -> List ( String, List a )
flatten located =
    case located of
        Pure v ->
            [ ( "", [ v ] ) ]

        InField s vals ->
            gather ("/" ++ s) vals

        AtIndex i vals ->
            gather ("/" ++ Basics.toString i) vals


gather : String -> Nonempty (Located a) -> List ( String, List a )
gather prefix (Nonempty first rest) =
    List.concatMap flatten (first :: rest)
        |> group
        |> List.map (Tuple.mapFirst ((++) prefix))


group : List ( String, List a ) -> List ( String, List a )
group =
    List.foldr
        (\( key, item ) { keys, items } ->
            if Set.member key keys then
                { keys = keys
                , items =
                    List.map
                        (\( k, v ) ->
                            if k == key then
                                ( k, item ++ v )
                            else
                                ( k, v )
                        )
                        items
                }
            else
                { keys = Set.insert key keys
                , items = ( key, item ) :: items
                }
        )
        { keys = Set.empty, items = [] }
        >> .items


map : (a -> b) -> Located a -> Located b
map op located =
    case located of
        InField f val ->
            InField f <| Nonempty.map (map op) val

        AtIndex i val ->
            AtIndex i <| Nonempty.map (map op) val

        Pure v ->
            Pure (op v)
