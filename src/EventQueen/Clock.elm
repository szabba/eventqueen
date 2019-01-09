--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.Clock exposing (Clock, PartialOrder(..), compare, compareTotal, fromDict, tick, ticksOf, toDict, zero)

import Dict exposing (Dict)
import Set


type Clock
    = Clock (Dict String Int)


type PartialOrder
    = Chronological
    | ReverseChronological
    | Concurrent
    | Simultaneous


compare : ( Clock, Clock ) -> PartialOrder
compare ( left, right ) =
    let
        allNodes =
            [ left |> toDict |> Dict.keys
            , right |> toDict |> Dict.keys
            ]
                |> List.concat
                |> Set.fromList
                |> Set.toList

        tickOrderings =
            allNodes |> List.map (compareTicks left right)

        anyLess =
            tickOrderings |> List.any ((==) LT)

        anyMore =
            tickOrderings |> List.any ((==) GT)
    in
    case ( anyLess, anyMore ) of
        ( False, False ) ->
            Simultaneous

        ( True, False ) ->
            Chronological

        ( False, True ) ->
            ReverseChronological

        ( True, True ) ->
            Concurrent


{-| This function defines an artificial total order on clocks.
-}
compareTotal : Clock -> Clock -> Order
compareTotal (Clock left) (Clock right) =
    -- TODO: use in MVRs
    let
        allKeys =
            (Dict.keys left ++ Dict.keys right)
                |> Set.fromList
                |> Set.toList

        ticks dict =
            allKeys |> List.map (\k -> dict |> Dict.get k |> Maybe.withDefault 0)
    in
    Basics.compare (ticks left) (ticks right)


compareTicks left right node =
    Basics.compare
        (left |> ticksOf { node = node })
        (right |> ticksOf { node = node })


zero : Clock
zero =
    Clock Dict.empty


toDict : Clock -> Dict String Int
toDict (Clock dict) =
    dict


fromDict : Dict String Int -> Clock
fromDict dict =
    dict
        |> Dict.filter (always <| (<) 0)
        |> Clock


ticksOf : { node : String } -> Clock -> Int
ticksOf { node } (Clock ticks) =
    ticks
        |> Dict.get node
        |> Maybe.withDefault 0


tick : { node : String } -> Clock -> Clock
tick { node } (Clock ticks) =
    ticks
        |> Dict.update node (Maybe.withDefault 0 >> (+) 1 >> Just)
        |> Clock
