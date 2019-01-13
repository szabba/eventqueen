module Tests.EventQueen.MultivalueRegister exposing (suite)

import Dict
import EventQueen.Clock as Clock exposing (Clock)
import EventQueen.MultivalueRegister as MVR exposing (Diff, MVR)
import EventQueen.Node as Node
import EventQueen.Node.Simulation as Simulation
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tests.EventQueen.Consistency as Consistency


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
                Node.init config { name = "node" }
                    |> Node.update config (MVR.set [ 1, 2, 3 ])
                    |> .state
                    |> MVR.get
                    |> Expect.equal [ 1, 2, 3 ]
        , test "the set values are sorted as per the config" <|
            \() ->
                Node.init config { name = "node" }
                    |> Node.update config (MVR.set [ 1, 3, 2 ])
                    |> .state
                    |> MVR.get
                    |> Expect.equal [ 1, 2, 3 ]
        , test "newer values shadow older ones" <|
            \() ->
                Node.init config { name = "node" }
                    |> Node.update config (MVR.set [ 1, 3, 2 ])
                    |> Node.update config (MVR.set [ 5, 4 ])
                    |> .state
                    |> MVR.get
                    |> Expect.equal [ 4, 5 ]
        , Consistency.isStronglyEventuallyConsistent
            { nodes = 3
            , operation =
                Fuzz.map Set <| Fuzz.list Fuzz.int
            , simulation =
                { node = config
                , apply = \(Set values) -> MVR.set values
                }
            }
        ]


type Operation
    = Set (List Int)


config : Node.Config (MVR.Diff Int) (MVR Int)
config =
    MVR.config compare
