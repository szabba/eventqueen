--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.Clock exposing (Clock, fromDict, tick, ticksOf, toDict, zero)

import Dict exposing (Dict)


type Clock
    = Clock (Dict String Int)


zero : Clock
zero =
    Clock Dict.empty


toDict : Clock -> Dict String Int
toDict (Clock dict) =
    dict


fromDict : Dict String Int -> Clock
fromDict dict =
    dict
        |> Dict.filter (\_ v -> v >= 0)
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
