--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.MultivalueRegister exposing (Diff, MVR, MultivalueRegister, config, get, init, set)

import EventQueen.Clock as Clock exposing (Clock)
import EventQueen.Node as Node exposing (Config)


type MultivalueRegister a
    = MultivalueRegister (List (Entry a))


type alias MVR a =
    MultivalueRegister a


type alias Diff a =
    ( List a, Clock )


type alias Entry a =
    { clock : Clock
    , value : a
    }


config : (a -> a -> Order) -> Config (Diff a) (MultivalueRegister a)
config compareWith =
    { init = init
    , patch = patch compareWith
    }


init : MultivalueRegister a
init =
    MultivalueRegister []



-- QUERIES


get : MultivalueRegister a -> List a
get (MultivalueRegister entries) =
    entries
        |> List.map .value



-- OPERATIONS


set : List a -> Node.Operation (Diff a) (MultivalueRegister a)
set =
    rawSet >> Node.operation


rawSet : List a -> { name : String } -> Clock -> MultivalueRegister a -> Diff a
rawSet values { name } clock _ =
    ( values, clock |> Clock.tick { node = name } )



-- PATCH


patch : (a -> a -> Order) -> Diff a -> MultivalueRegister a -> MultivalueRegister a
patch compareWith ( values, clock ) (MultivalueRegister entries) =
    entries
        |> List.filter (not << isOlderThan clock)
        |> appendConcurrent clock values
        |> fixOrdering compareWith
        |> MultivalueRegister


appendConcurrent : Clock -> List a -> List (Entry a) -> List (Entry a)
appendConcurrent clock candidates entries =
    if entries |> List.all (isConcurrentTo clock) then
        List.map (Entry clock) candidates ++ entries

    else
        entries


fixOrdering : (a -> a -> Order) -> List (Entry a) -> List (Entry a)
fixOrdering compareWith =
    List.sortWith (compareEntries compareWith)


compareEntries : (a -> a -> Order) -> Entry a -> Entry a -> Order
compareEntries compareWith left right =
    case Clock.compareTotal left.clock right.clock of
        EQ ->
            compareWith left.value right.value

        order ->
            order


isOlderThan : Clock -> Entry a -> Bool
isOlderThan clock entry =
    Clock.compare ( entry.clock, clock ) == Clock.Chronological


isConcurrentTo : Clock -> Entry a -> Bool
isConcurrentTo clock entry =
    Clock.compare ( clock, entry.clock ) == Clock.Concurrent
