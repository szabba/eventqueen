--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.Node.Simulation exposing (Config, Event(..), ensureFullSync, run)

import Array
import Dict exposing (Dict)
import EventQueen.Clock as Clock exposing (Clock)
import EventQueen.Node as Node exposing (Node)
import Set exposing (Set)


type Event operation
    = Apply { atNode : String, operation : operation }
    | Sync { fromNode : String, toNode : String }


type alias Config operation diff state =
    { apply : operation -> { name : String } -> Clock -> state -> diff
    , node : Node.Config diff state
    }


run : Config operation diff state -> List (Event operation) -> Dict String (Node diff state)
run config events =
    events
        |> List.foldl (\event nodes -> nodes |> runOnce config event) Dict.empty


ensureFullSync : List (Event operation) -> List (Event operation)
ensureFullSync events =
    let
        nodeNames =
            events |> historyNodeNames
    in
    events ++ gather nodeNames ++ spread nodeNames


runOnce : Config operation diff state -> Event operation -> Dict String (Node diff state) -> Dict String (Node diff state)
runOnce config evt system =
    case evt of
        Apply params ->
            system |> apply config params

        Sync params ->
            system |> syncFromTo config params


apply : Config operation diff state -> { atNode : String, operation : operation } -> Dict String (Node diff state) -> Dict String (Node diff state)
apply config { atNode, operation } system =
    let
        updateNode =
            Maybe.withDefault (Node.init config.node { name = atNode })
                >> Node.update config.node (config.apply operation)
                >> Just
    in
    system |> Dict.update atNode updateNode


syncFromTo : Config operation diff state -> { fromNode : String, toNode : String } -> Dict String (Node diff state) -> Dict String (Node diff state)
syncFromTo config { fromNode, toNode } system =
    let
        targetClock =
            system
                |> Dict.get toNode
                |> Maybe.map .clock
                |> Maybe.withDefault Clock.zero

        missingChanges =
            system
                |> Dict.get fromNode
                |> Maybe.map (Node.diff targetClock)
                |> Maybe.withDefault Array.empty

        updatedNode =
            system
                |> Dict.get toNode
                |> Maybe.withDefault (Node.init config.node { name = toNode })
                |> Node.patch config.node missingChanges
    in
    system |> Dict.insert toNode updatedNode


gather : List String -> List (Event noOps)
gather nodeNames =
    case nodeNames of
        first :: rest ->
            rest |> List.map (\other -> Sync { fromNode = other, toNode = first })

        [] ->
            []


spread : List String -> List (Event noOps)
spread nodeNames =
    case nodeNames of
        first :: rest ->
            rest |> List.map (\other -> Sync { fromNode = first, toNode = other })

        [] ->
            []


historyNodeNames : List (Event operation) -> List String
historyNodeNames =
    List.concatMap eventNodeNames >> Set.fromList >> Set.toList


eventNodeNames : Event operation -> List String
eventNodeNames evt =
    case evt of
        Apply { atNode } ->
            [ atNode ]

        Sync { fromNode, toNode } ->
            [ fromNode, toNode ]
