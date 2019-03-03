module Tests.EventQueen.Consistency exposing (Config, isStronglyEventuallyConsistent)

import Dict
import EventQueen.Node.Simulation as Simulation
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Tests.EventQueen.Simulation.History as History


type alias Config operation diff state =
    { simulation : Simulation.Config operation diff state
    , history : History.Config operation
    }


isStronglyEventuallyConsistent : Config operation diff state -> Test
isStronglyEventuallyConsistent config =
    Test.fuzz (history config) "is strongly eventually consistent" <|
        \events ->
            events
                |> Simulation.run config.simulation
                |> Dict.values
                |> List.map .state
                |> allEqual


history : Config operation diff state -> Fuzzer (List (Simulation.Event operation))
history config =
    History.fuzz config.history
        |> Fuzz.map History.ensureFullSync


event : { a | nodes : Int, operation : Fuzzer operation } -> Fuzzer (Simulation.Event operation)
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


allEqual : List a -> Expectation
allEqual vals =
    let
        consMissing v acc =
            if acc |> List.member v then
                acc

            else
                v :: acc

        uniqueVals =
            vals |> List.foldl consMissing []

        failureMessage n =
            case n of
                0 ->
                    Nothing

                1 ->
                    Nothing

                _ ->
                    Just <| "There are several different values: " ++ Debug.toString uniqueVals ++ "."
    in
    uniqueVals
        |> List.length
        |> failureMessage
        |> Maybe.map Expect.fail
        |> Maybe.withDefault Expect.pass
