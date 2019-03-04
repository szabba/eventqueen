module Tests.EventQueen.Table exposing (suite)

import EventQueen.MultivalueRegister as MVR exposing (MVR)
import EventQueen.Node as Node exposing (Node)
import EventQueen.Node.Simulation as Simulation
import EventQueen.Table as Table exposing (Table)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Maybe.Extra as Maybe
import Result.Extra as Result
import Test exposing (..)
import Tests.EventQueen.Consistency as Consistency
import Tests.EventQueen.Simulation.History as History


type alias State =
    Table (MVR Int)


type alias Diff =
    Table.Diff (MVR.Diff Int)


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
        , fuzz randomNode "a freshly added entry has the initial value" <|
            \startNode ->
                let
                    withEntry =
                        startNode
                            |> Node.update config.node Table.add
                            |> .state
                in
                withEntry
                    |> Table.lastID { node = startNode.name }
                    |> Result.fromMaybe "failed to get the last ID added"
                    |> Result.andThen
                        (\id ->
                            withEntry
                                |> Table.get id
                                |> Result.fromMaybe "failed to get the last entry"
                        )
                    |> Result.map (Expect.equal MVR.init)
                    |> Result.extract Expect.fail
        , fuzz randomNode "remove shrinks a non-empty table" <|
            \startNode ->
                let
                    withEntry =
                        startNode |> Node.update config.node Table.add
                in
                withEntry.state
                    |> Table.lastID { node = startNode.name }
                    |> Result.fromMaybe "failed to get the last ID added"
                    |> Result.map
                        (\id ->
                            withEntry
                                |> Node.update config.node (Table.remove id)
                                |> .state
                                |> Table.get id
                        )
                    |> Result.map (Expect.equal Nothing)
                    |> Result.extract Expect.fail
        , fuzz randomNode "update modifies the entry mentioned" <|
            \startNode ->
                let
                    withEntry =
                        startNode |> Node.update config.node Table.add
                in
                withEntry.state
                    |> Table.lastID { node = startNode.name }
                    |> Result.fromMaybe "failed to get the last ID added"
                    |> Result.andThen
                        (\id ->
                            withEntry
                                |> Node.update config.node (Table.update id <| MVR.set [ 1, 2, 3 ])
                                |> .state
                                |> Table.get id
                                |> Result.fromMaybe "failed to get the last entry"
                                |> Result.map MVR.get
                        )
                    |> Result.map (Expect.equal [ 1, 2, 3 ])
                    |> Result.extract Expect.fail
        , fuzz randomNode "a later update does not invalidate a removal" <|
            \startNode ->
                let
                    withEntry =
                        startNode |> Node.update config.node Table.add
                in
                withEntry.state
                    |> Table.lastID { node = startNode.name }
                    |> Result.fromMaybe "failed to get the last ID added"
                    |> Result.map
                        (\id ->
                            withEntry
                                |> Node.update config.node (Table.remove id)
                                |> Node.update config.node (Table.update id <| MVR.set [ 1, 2, 3 ])
                                |> .state
                                |> Table.get id
                        )
                    |> Result.map (Expect.equal Nothing)
                    |> Result.extract Expect.fail
        , Consistency.concurrently "update wins with concurrent removal"
            { history =
                { nodes = 3
                , operation = operation
                }
            , simulation = config
            }
            (\{ root } ->
                { atRoot = addOne >> Ok
                , atLeft = removeLast { from = root }
                , atRight = updateLast { from = root } [ 1, 2, 3 ]
                , expectMerge = lastValue { from = root } [ 1, 2, 3 ]
                }
            )
        , Consistency.isStronglyEventuallyConsistent
            { history =
                { nodes = 3
                , operation = operation
                }
            , simulation = config
            }
        ]


lastValue : { from : String } -> List Int -> Node Diff State -> Expectation
lastValue { from } expected node =
    node.state
        |> Table.lastID { node = from }
        |> Result.fromMaybe "no last ID to assert on"
        |> Result.map
            (\id ->
                node.state
                    |> Table.get id
                    |> Maybe.map MVR.get
                    |> Expect.equal (Just expected)
            )
        |> Result.extract Expect.fail


addOne : Node Diff State -> Node Diff State
addOne node =
    node |> Node.update config.node Table.add


removeLast : { from : String } -> Node Diff State -> Result String (Node Diff State)
removeLast { from } node =
    node.state
        |> Table.lastID { node = from }
        |> Result.fromMaybe "no last ID to remove"
        |> Result.map (\id -> node |> Node.update config.node (Table.remove id))


updateLast : { from : String } -> List Int -> Node Diff State -> Result String (Node Diff State)
updateLast { from } newValues node =
    node.state
        |> Table.lastID { node = from }
        |> Result.fromMaybe "no last ID to update"
        |> Result.map (\id -> node |> Node.update config.node (Table.update id (MVR.set newValues)))


pending : Expectation
pending =
    Expect.fail "TODO"


randomNode =
    History.fuzzNode
        { nodes = 3
        , operation = operation
        }
        config


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
