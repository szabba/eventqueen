module Tests.EventQueen.Simulation.History exposing (Config, ensureFullSync, fuzz, fuzzNode)

import Dict
import EventQueen.Node as Node exposing (Node)
import EventQueen.Node.Simulation as Simulation
import Fuzz exposing (Fuzzer)
import List.Extra as List
import Set



-- FUZZ


type alias Config operation =
    { nodes : Int
    , operation : Fuzzer operation
    }



-- FUZZ


fuzz : Config operation -> Fuzzer (List (Simulation.Event operation))
fuzz config =
    Fuzz.list (event config)


event : Config operation -> Fuzzer (Simulation.Event operation)
event { nodes, operation } =
    Fuzz.oneOf
        [ Fuzz.map2 applyEvent (Fuzz.intRange 0 nodes) operation
        , Fuzz.map2 syncEvent (Fuzz.intRange 0 nodes) (Fuzz.intRange 0 nodes)
        ]


applyEvent : Int -> operation -> Simulation.Event operation
applyEvent nodeNo op =
    Simulation.Apply
        { atNode = String.fromInt nodeNo
        , operation = op
        }


syncEvent : Int -> Int -> Simulation.Event operation
syncEvent fromNodeNo toNodeNo =
    Simulation.Sync
        { fromNode = String.fromInt fromNodeNo
        , toNode = String.fromInt toNodeNo
        }



-- FUZZ NODE


fuzzNode : Config operation -> Simulation.Config operation diff state -> Fuzzer (Node diff state)
fuzzNode config simConfig =
    let
        target =
            syncTarget config
    in
    config
        |> fuzz
        |> Fuzz.map (syncAllTo target config.nodes)
        |> Fuzz.map (Simulation.run simConfig)
        |> Fuzz.map (Dict.get target)
        |> Fuzz.map (Maybe.withDefault <| Node.init simConfig.node { name = target })


syncAllTo : String -> Int -> List (Simulation.Event operation) -> List (Simulation.Event operation)
syncAllTo target nodes past =
    past ++ List.initialize (nodes + 1) (syncOneTo target)


syncOneTo : String -> Int -> Simulation.Event operation
syncOneTo target fromN =
    Simulation.Sync
        { fromNode = fromN |> String.fromInt
        , toNode = target
        }


syncTarget : Config operation -> String
syncTarget { nodes } =
    nodes + 1 |> String.fromInt



-- ENSURE FULL SYNC


ensureFullSync : List (Simulation.Event operation) -> List (Simulation.Event operation)
ensureFullSync events =
    let
        nodeNames =
            events |> historyNodeNames
    in
    events ++ gather nodeNames ++ spread nodeNames


gather : List String -> List (Simulation.Event noOps)
gather nodeNames =
    case nodeNames of
        first :: rest ->
            rest |> List.map (\other -> Simulation.Sync { fromNode = other, toNode = first })

        [] ->
            []


spread : List String -> List (Simulation.Event noOps)
spread nodeNames =
    case nodeNames of
        first :: rest ->
            rest |> List.map (\other -> Simulation.Sync { fromNode = first, toNode = other })

        [] ->
            []


historyNodeNames : List (Simulation.Event operation) -> List String
historyNodeNames =
    List.concatMap eventNodeNames >> Set.fromList >> Set.toList


eventNodeNames : Simulation.Event operation -> List String
eventNodeNames evt =
    case evt of
        Simulation.Apply { atNode } ->
            [ atNode ]

        Simulation.Sync { fromNode, toNode } ->
            [ fromNode, toNode ]
