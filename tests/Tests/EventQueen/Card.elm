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


suite : Test
suite =
    describe "EventQueen.Card"
        [ fuzz (history { nodes = 3 } |> Fuzz.map Simulation.ensureFullSync) "is strongly eventually consistent" <|
            \events ->
                events
                    |> Simulation.run config
                    |> Dict.values
                    |> List.map .state
                    |> allEqual
        ]


history : { nodes : Int } -> Fuzzer (List (Simulation.Event Operation))
history nodes =
    list (event nodes)


event : { nodes : Int } -> Fuzzer (Simulation.Event Operation)
event { nodes } =
    Fuzz.oneOf
        [ Fuzz.map2 applyEvent (Fuzz.intRange 0 nodes) operation
        , Fuzz.map2 syncEvent (Fuzz.intRange 0 nodes) (Fuzz.intRange 0 nodes)
        ]


applyEvent : Int -> Operation -> Simulation.Event Operation
applyEvent nodeNo op =
    Simulation.Apply
        { atNode = String.fromInt nodeNo
        , operation = op
        }


syncEvent : Int -> Int -> Simulation.Event Operation
syncEvent fromNodeNo toNodeNo =
    Simulation.Sync
        { fromNode = String.fromInt fromNodeNo
        , toNode = String.fromInt toNodeNo
        }


type Operation
    = SetText String
    | SetPosition ( Float, Float )


operation : Fuzzer Operation
operation =
    Fuzz.oneOf
        [ Fuzz.map SetText Fuzz.string
        , Fuzz.map SetPosition <| Fuzz.map2 Tuple.pair Fuzz.float Fuzz.float
        ]


config : Simulation.Config Operation Diff Card
config =
    { node = Card.config
    , apply = apply
    }


apply : Operation -> { name : String } -> Clock -> Card -> Diff
apply op =
    case op of
        SetText text ->
            Card.setText text

        SetPosition pos ->
            Card.setPosition pos


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
