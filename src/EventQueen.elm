--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen exposing (main)

import Browser
import Html


main : Program Flags Model Never
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { rawNodeID : String }


type alias Model =
    { nodeID : NodeID }


type NodeID
    = NodeID String


init { rawNodeID } =
    ( Model <| NodeID rawNodeID
    , Cmd.none
    )


view _ =
    { title = "Event Queen"
    , body = [ Html.text "event queen" ]
    }


update : Never -> Model -> ( Model, Cmd Never )
update _ model =
    ( model
    , Cmd.none
    )


subscriptions _ =
    Sub.none
