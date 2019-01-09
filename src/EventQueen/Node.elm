--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.Node exposing (Change, Config, Node, diff, init, patch, update)

import Array exposing (Array)
import EventQueen.Clock as Clock exposing (Clock)


type alias Config diff state =
    { init : state
    , patch : diff -> state -> state
    }


type alias Node diff state =
    { name : String
    , clock : Clock
    , state : state
    , changes : Array (Change diff)
    }


type alias Change diff =
    { atNode : String
    , tick : Int
    , diff : diff
    }


init : Config diff state -> { name : String } -> Node diff state
init config { name } =
    { name = name
    , clock = Clock.zero
    , state = config.init
    , changes = Array.empty
    }


update : Config diff state -> ({ name : String } -> Clock -> state -> diff) -> Node diff state -> Node diff state
update config updateFunc node =
    let
        nextDiff =
            node.state |> updateFunc { name = node.name } node.clock

        nextClock =
            node.clock |> Clock.tick { node = node.name }

        newState =
            node.state |> config.patch nextDiff

        newChanges =
            node.changes
                |> Array.push
                    { atNode = node.name
                    , tick = nextClock |> Clock.ticksOf { node = node.name }
                    , diff = nextDiff
                    }
    in
    { node
        | clock = nextClock
        , state = newState
        , changes = newChanges
    }


diff : Clock -> Node diff state -> Array (Change diff)
diff remoteClock { changes } =
    changes
        |> Array.filter (missingAt remoteClock)


patch : Config diff state -> Array (Change diff) -> Node diff state -> Node diff state
patch config extraChanges node =
    extraChanges
        |> Array.foldl (patchOne config) node


missingAt : Clock -> Change diff -> Bool
missingAt clock { atNode, tick } =
    tick > (clock |> Clock.ticksOf { node = atNode })


patchOne : Config diff state -> Change diff -> Node diff state -> Node diff state
patchOne config change node =
    if not <| isNextAt node.clock change then
        node

    else
        let
            newClock =
                node.clock |> Clock.tick { node = change.atNode }

            newState =
                node.state |> config.patch change.diff

            newChanges =
                node.changes |> Array.push change
        in
        { node
            | clock = newClock
            , state = newState
            , changes = newChanges
        }


isNextAt : Clock -> Change diff -> Bool
isNextAt clock { atNode, tick } =
    tick == 1 + (clock |> Clock.ticksOf { node = atNode })
