--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.Table exposing (Diff, ID, Table, add, config, get, init, lastID, noop, remove, toList, update)

import Dict exposing (Dict)
import EventQueen.Clock as Clock exposing (Clock)
import EventQueen.Node as Node
import Maybe.Extra as Maybe


type Table value
    = Table
        { lastIDs : Clock
        , entries : Dict ( String, Int ) value
        }


type ID
    = ID ( String, Int )


type Diff diff
    = NoOp
    | Add { node : String }
    | Remove ID
    | Update ID diff


config : Node.Config diff value -> Node.Config (Diff diff) (Table value)
config entryConfig =
    { init = init
    , patch = patch entryConfig
    }


init : Table value
init =
    Table { lastIDs = Clock.zero, entries = Dict.empty }



-- QUERIES


lastID : { node : String } -> Table value -> Maybe ID
lastID nodeID (Table { lastIDs }) =
    lastIDs
        |> Clock.ticksOf nodeID
        |> Just
        |> Maybe.filter ((/=) 0)
        |> Maybe.map (Tuple.pair nodeID.node >> ID)


get : ID -> Table value -> Maybe value
get (ID rawID) (Table { entries }) =
    entries |> Dict.get rawID


toList : Table value -> List ( ID, value )
toList (Table { entries }) =
    entries
        |> Dict.toList
        |> List.map (Tuple.mapFirst ID)



-- OPERATIONS


noop : Node.Operation (Diff diff) (Table value)
noop =
    -- TODO: replace with a generic Operation.noop
    Node.operation <| \_ _ _ -> NoOp


add : Node.Operation (Diff diff) (Table value)
add =
    Node.operation <| \{ name } _ _ -> Add { node = name }


remove : ID -> Node.Operation (Diff diff) (Table value)
remove id =
    Node.operation <| \_ _ _ -> Remove id


update : ID -> Node.Operation diff value -> Node.Operation (Diff diff) (Table value)
update id entryOperation =
    Node.operation <|
        \nodeID clock table ->
            table
                |> get id
                |> Maybe.map (Node.runOperation nodeID clock entryOperation)
                |> Maybe.map (Update id)
                |> Maybe.withDefault NoOp



-- PATCH


patch : Node.Config diff value -> Diff diff -> Table value -> Table value
patch entryConfig diff table =
    case diff of
        NoOp ->
            table

        Remove id ->
            table |> removeEntry id

        Add nodeID ->
            table |> addEntry nodeID entryConfig.init

        Update id entryDiff ->
            table |> updateEntry id (entryConfig.patch entryDiff)


addEntry : { node : String } -> value -> Table value -> Table value
addEntry nodeID value table =
    let
        (Table { lastIDs, entries }) =
            table

        nextIDs =
            lastIDs |> Clock.tick nodeID

        (ID nextEntryID) =
            table
                |> lastID nodeID
                |> Maybe.map nextID
                |> Maybe.withDefault (firstID nodeID)

        newEntries =
            entries |> Dict.insert nextEntryID value
    in
    Table { lastIDs = nextIDs, entries = newEntries }


removeEntry : ID -> Table value -> Table value
removeEntry (ID rawID) (Table ({ entries } as guts)) =
    let
        newEntries =
            entries |> Dict.remove rawID
    in
    Table { guts | entries = newEntries }


updateEntry : ID -> (value -> value) -> Table value -> Table value
updateEntry (ID rawID) updateFunc (Table ({ entries } as guts)) =
    let
        newEntries =
            entries |> Dict.update rawID (Maybe.map updateFunc)
    in
    Table { guts | entries = newEntries }


firstID : { node : String } -> ID
firstID { node } =
    ID ( node, 1 )


nextID : ID -> ID
nextID (ID ( node, n )) =
    ID ( node, n + 1 )
