--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.Node.Simulation exposing (Config, Event(..), run, runFromState, syncFrom)

import Array
import Dict exposing (Dict)
import EventQueen.Clock as Clock exposing (Clock)
import EventQueen.Node as Node exposing (Node)
import Set exposing (Set)


type Event operation
    = Apply { atNode : String, operation : operation }
    | Sync { fromNode : String, toNode : String }


type alias Config operation diff state =
    { apply : operation -> state -> Node.Operation diff state
    , node : Node.Config diff state
    }


run : Config operation diff state -> List (Event operation) -> Dict String (Node diff state)
run config events =
    Dict.empty |> runFromState config events


runFromState : Config operation diff state -> List (Event operation) -> Dict String (Node diff state) -> Dict String (Node diff state)
runFromState config events startNodes =
    events
        |> List.foldl (\event nodes -> nodes |> runOnce config event) startNodes


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
        updateExisting node =
            node
                |> Node.update config.node (config.apply operation node.state)

        updateNode =
            Maybe.withDefault (Node.init config.node { name = atNode })
                >> updateExisting
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


syncFrom : Node.Config diff state -> Node diff state -> Node diff state -> Node diff state
syncFrom config from to =
    let
        missingChanges =
            from |> Node.diff to.clock
    in
    to |> Node.patch config missingChanges
