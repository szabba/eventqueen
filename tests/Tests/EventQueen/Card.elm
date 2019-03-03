module Tests.EventQueen.Card exposing (suite)

import Dict
import EventQueen.Card as Card exposing (Card, Diff)
import EventQueen.Clock as Clock exposing (Clock)
import EventQueen.MultivalueRegister as MVR exposing (MVR)
import EventQueen.Node as Node
import EventQueen.Node.Simulation as Simulation
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tests.EventQueen.Consistency as Consistency


suite : Test
suite =
    describe "EventQueen.Card"
        [ Consistency.isStronglyEventuallyConsistent
            { history =
                { nodes = 3
                , operation = operation
                }
            , simulation = config
            }
        ]


type Operation
    = SetText String
    | SetPosition ( Float, Float )


operation : Fuzzer Operation
operation =
    Fuzz.oneOf
        [ Fuzz.map SetText Fuzz.string
        , Fuzz.map SetPosition <| Fuzz.map2 Tuple.pair Fuzz.float Fuzz.float
        ]


config : Simulation.Config Operation Card.Diff Card
config =
    { node = Card.config
    , apply =
        \op _ ->
            case op of
                SetText text ->
                    Card.setText text

                SetPosition pos ->
                    Card.setPosition pos
    }
