module Tests.EventQueen.MultivalueRegister exposing (suite)

import Dict
import EventQueen.Clock as Clock exposing (Clock)
import EventQueen.MultivalueRegister as MVR exposing (Diff, MVR)
import EventQueen.Node as Node
import EventQueen.Node.Simulation as Simulation
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "EventQueen.MultivalueRegister"
        [ test "the init register holds no values" <|
            \() ->
                MVR.init
                    |> MVR.get
                    |> Expect.equal []
        , test "set on a starting node sets the specified values" <|
            \() ->
                Node.init config.node { name = "node" }
                    |> Node.update config.node (MVR.set [ 1, 2, 3 ])
                    |> .state
                    |> MVR.get
                    |> Expect.equal [ 1, 2, 3 ]
        , test "the set values are sorted as per the config" <|
            \() ->
                Node.init config.node { name = "node" }
                    |> Node.update config.node (MVR.set [ 1, 3, 2 ])
                    |> .state
                    |> MVR.get
                    |> Expect.equal [ 1, 2, 3 ]
        , fuzz (history { nodes = 3 } |> Fuzz.map Simulation.ensureFullSync) "is strongly eventually consistent" <|
            \events ->
                events
                    |> Simulation.run config
                    |> Dict.values
                    |> List.map (.state >> MVR.get)
                    |> allEqual
        ]


history : { nodes : Int } -> Fuzzer (List (Simulation.Event Operation))
history nodes =
    list (event nodes)


event : { nodes : Int } -> Fuzzer (Simulation.Event Operation)
event { nodes } =
    Fuzz.oneOf
        [ Fuzz.map2 applyEvent (Fuzz.intRange 0 nodes) (list int)
        , Fuzz.map2 syncEvent (Fuzz.intRange 0 nodes) (Fuzz.intRange 0 nodes)
        ]


applyEvent : Int -> List Int -> Simulation.Event Operation
applyEvent nodeNo values =
    Simulation.Apply
        { atNode = String.fromInt nodeNo
        , operation = Set values
        }


syncEvent : Int -> Int -> Simulation.Event Operation
syncEvent fromNodeNo toNodeNo =
    Simulation.Sync
        { fromNode = String.fromInt fromNodeNo
        , toNode = String.fromInt toNodeNo
        }


type Operation
    = Set (List Int)


config : Simulation.Config Operation (Diff Int) (MVR Int)
config =
    { node = MVR.config compare
    , apply = \(Set strings) -> MVR.set strings
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
