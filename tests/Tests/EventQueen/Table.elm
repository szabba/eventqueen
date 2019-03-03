module Tests.EventQueen.Table exposing (suite)

import EventQueen.MultivalueRegister as MVR exposing (MVR)
import EventQueen.Node as Node
import EventQueen.Node.Simulation as Simulation
import EventQueen.Table as Table exposing (Table)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Maybe.Extra as Maybe
import Test exposing (..)
import Tests.EventQueen.Consistency as Consistency


suite : Test
suite =
    describe "EventQueen.Table"
        [ test "the init table has no entries" <|
            \() ->
                Table.init
                    |> Table.toList
                    |> Expect.equal []
        , test "a table knows the ID of the last inserted entry" <|
            \() ->
                Node.init config.node { name = "node" }
                    |> Node.update config.node Table.add
                    |> .state
                    |> Table.lastID { node = "node" }
                    |> Maybe.isJust
                    |> Expect.true "there should be a last assigned ID"
        , fuzz (Fuzz.intRange 0 3) "a table contains all unremoved entries" <|
            \times ->
                Node.init config.node { name = "node" }
                    |> repeat times (Node.update config.node Table.add)
                    |> .state
                    |> Table.toList
                    |> List.map Tuple.second
                    |> Expect.equal (List.repeat times MVR.init)
        , Consistency.isStronglyEventuallyConsistent
            { history =
                { nodes = 3
                , operation = operation
                }
            , simulation = config
            }
        ]


repeat : Int -> (a -> a) -> (a -> a)
repeat n f x =
    if n > 0 then
        repeat (n - 1) f (f x)

    else
        x


type Operation
    = Add
    | Remove Float
    | Update Float (List Int)


config : Simulation.Config Operation (Table.Diff (MVR.Diff Int)) (Table (MVR Int))
config =
    { apply = apply
    , node = Table.config (MVR.config compare)
    }


operation : Fuzzer Operation
operation =
    Fuzz.oneOf
        [ Fuzz.constant Add
        , Fuzz.map Remove Fuzz.float
        , Fuzz.map2 Update Fuzz.float (Fuzz.list Fuzz.int)
        ]


apply : Operation -> Table (MVR Int) -> Node.Operation (Table.Diff (MVR.Diff Int)) (Table (MVR Int))
apply op table =
    case op of
        Add ->
            Table.add

        Remove fraction ->
            table
                |> idAtFraction fraction
                |> Maybe.map Table.remove
                |> Maybe.withDefault Table.noop

        Update fraction newValues ->
            table
                |> idAtFraction fraction
                |> Maybe.map (\id -> Table.update id (MVR.set newValues))
                |> Maybe.withDefault Table.noop


idAtFraction : Float -> Table value -> Maybe Table.ID
idAtFraction fraction table =
    let
        ids =
            table
                |> Table.toList
                |> List.map Tuple.first

        idIx =
            floor <| toFloat (List.length ids) * fraction
    in
    ids
        |> List.drop idIx
        |> List.head
