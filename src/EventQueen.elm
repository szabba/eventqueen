--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen exposing (main)

import Browser
import Browser.Events
import Element exposing (Element)
import Element.Events as Events
import Html exposing (Html)
import Html.Events as HE
import Json.Decode as Decode exposing (Decoder)


main : Program Flags Model Msg
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
    { nodeID : NodeID
    , offset : ( Float, Float )
    , dragState : DragState
    }


type Msg
    = NoOp
    | StartBoardDrag
    | DragBoard ( Float, Float )
    | StopDrag


type NodeID
    = NodeID String


type DragState
    = NotDragging
    | DraggingBoard


init : Flags -> ( Model, Cmd Msg )
init { rawNodeID } =
    ( { nodeID = NodeID rawNodeID
      , offset = ( 0, 0 )
      , dragState = NotDragging
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Event Queen"
    , body =
        Element.text "event queen"
            |> onBoard model
    }


onBoard : { a | offset : ( Float, Float ) } -> Element Msg -> List (Html Msg)
onBoard ({ offset } as model) =
    let
        ( dx, dy ) =
            offset
    in
    List.singleton
        << Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Events.onMouseDown StartBoardDrag
            , Events.onMouseUp StopDrag
            ]
        << Element.el
            [ Element.moveDown dy
            , Element.moveRight dx
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StopDrag ->
            ( { model | dragState = NotDragging }
            , Cmd.none
            )

        StartBoardDrag ->
            ( { model | dragState = DraggingBoard }
            , Cmd.none
            )

        DragBoard ( dx, dy ) ->
            let
                ( x, y ) =
                    model.offset
            in
            ( { model | offset = ( x + dx, y + dy ) }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        DraggingBoard ->
            drags DragBoard

        _ ->
            Sub.none


onMouseDownAt : (( Float, Float ) -> msg) -> Element.Attribute msg
onMouseDownAt toMsg =
    mouseAt
        |> Decode.map toMsg
        |> HE.on "mousedown"
        |> Element.htmlAttribute


drags : (( Float, Float ) -> msg) -> Sub msg
drags toMsg =
    mouseMoved
        |> Decode.map toMsg
        |> Browser.Events.onMouseMove


mouseMoved : Decoder ( Float, Float )
mouseMoved =
    Decode.map2 Tuple.pair
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


mouseAt : Decoder ( Float, Float )
mouseAt =
    Decode.map2 Tuple.pair
        (Decode.field "screenX" Decode.float)
        (Decode.field "screenY" Decode.float)
