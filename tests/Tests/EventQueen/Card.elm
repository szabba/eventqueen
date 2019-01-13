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
            { nodes = 3
            , operation =
                Fuzz.oneOf
                    [ Fuzz.map SetText Fuzz.string
                    , Fuzz.map SetPosition <| Fuzz.map2 Tuple.pair Fuzz.float Fuzz.float
                    ]
            , simulation =
                { node = Card.config
                , apply =
                    \op ->
                        case op of
                            SetText text ->
                                Card.setText text

                            SetPosition pos ->
                                Card.setPosition pos
                }
            }
        ]


type Operation
    = SetText String
    | SetPosition ( Float, Float )
