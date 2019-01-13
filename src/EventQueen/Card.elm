--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen.Card exposing (Card, Diff, config, getPosition, moveBy, setPosition, setText)

import EventQueen.Clock exposing (Clock)
import EventQueen.MultivalueRegister as MVR exposing (MVR)
import EventQueen.Node as Node exposing (Config)


type alias Card =
    { text : MVR String
    , position : MVR ( Float, Float )
    }


type Diff
    = TextDiff (MVR.Diff String)
    | PositionDiff (MVR.Diff ( Float, Float ))


config : Config Diff Card
config =
    { init = init
    , patch = patch
    }


init : Card
init =
    { text = MVR.init
    , position = MVR.init
    }


setText : String -> { name : String } -> Clock -> Card -> Diff
setText newText name clock card =
    card.text
        |> MVR.set [ newText ] name clock
        |> TextDiff


getPosition : Card -> ( Float, Float )
getPosition card =
    card.position
        |> MVR.get
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ( 0, 0 )


setPosition : ( Float, Float ) -> { name : String } -> Clock -> Card -> Diff
setPosition newPos name clock card =
    card.position
        |> MVR.set [ newPos ] name clock
        |> PositionDiff


moveBy : ( Float, Float ) -> { name : String } -> Clock -> Card -> Diff
moveBy ( dx, dy ) name clock card =
    let
        ( x, y ) =
            card |> getPosition
    in
    setPosition ( x + dx, y + dy ) name clock card


patch : Diff -> Card -> Card
patch diff card =
    case diff of
        TextDiff textDiff ->
            { card | text = card.text |> text.patch textDiff }

        PositionDiff posDiff ->
            { card | position = card.position |> position.patch posDiff }


text : Config (MVR.Diff String) (MVR String)
text =
    MVR.config compare


position : Config (MVR.Diff ( Float, Float )) (MVR ( Float, Float ))
position =
    MVR.config compare
