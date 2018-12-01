--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module EventQueen exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)
import Html.Attributes as HA
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
    , notes : Dict Int Note
    , nextNoteID : Int
    }


type Msg
    = NoOp
    | StartBoardDrag
    | DragBoard ( Float, Float )
    | StartNoteDrag Int
    | DragNote Int ( Float, Float )
    | StopDrag


type NodeID
    = NodeID String


type DragState
    = NotDragging
    | DraggingBoard
    | DraggingNote Int


type alias Note =
    { offset : ( Float, Float )
    , text : String
    }


init : Flags -> ( Model, Cmd Msg )
init { rawNodeID } =
    ( { nodeID = NodeID rawNodeID
      , offset = ( 0, 0 )
      , dragState = NotDragging
      , notes =
            Dict.fromList
                [ Tuple.pair 0 { offset = ( 0, 0 ), text = "Liberated" }
                , Tuple.pair 1 { offset = ( 50, 200 ), text = "Marched" }
                , Tuple.pair 2 { offset = ( 200, 50 ), text = "Forevered" }
                ]
      , nextNoteID = 0
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Event Queen"
    , body =
        onBoard model <| Element.el (List.map Element.inFront <| viewNotes model.notes) Element.none
    }


viewNotes : Dict Int Note -> List (Element Msg)
viewNotes =
    Dict.toList >> List.map viewNote


viewNote : ( Int, Note ) -> Element Msg
viewNote ( noteID, { offset, text } ) =
    withOffset offset <|
        Element.el
            [ Border.color noteBorder
            , Border.width 1
            , Background.color noteBackground
            , Element.padding 20
            , Element.width (Element.px 200)
            , Element.height (Element.px 200)
            , onMouseDownNoPropagation (StartNoteDrag noteID)
            , Element.htmlAttribute <| HA.style "user-select" "none"
            , Element.htmlAttribute <| HA.style "-moz-user-select" "none"
            , Element.htmlAttribute <| HA.style "-webkit-user-select" "none"
            , Element.htmlAttribute <| HA.style "-ms-user-select" "none"
            ]
        <|
            Element.paragraph []
                [ Element.text text
                ]


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
            , Background.color board
            ]
        << withOffset offset


withOffset : ( Float, Float ) -> Element msg -> Element msg
withOffset ( dx, dy ) =
    Element.el
        [ Element.moveDown dy
        , Element.moveRight dx
        ]


noteBackground : Element.Color
noteBackground =
    Element.rgb255 252 195 98


noteBorder : Element.Color
noteBorder =
    Element.rgb255 84 65 31


board : Element.Color
board =
    Element.rgb255 191 209 177


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

        StartNoteDrag noteID ->
            ( { model | dragState = DraggingNote noteID }
            , Cmd.none
            )

        DragNote noteID offsetDelta ->
            ( { model | notes = model.notes |> Dict.update noteID (Maybe.map <| updateOffsetBy offsetDelta) }
            , Cmd.none
            )


updateOffsetBy : ( Float, Float ) -> Note -> Note
updateOffsetBy ( dx, dy ) ({ offset } as note) =
    let
        ( x, y ) =
            offset
    in
    { note | offset = ( x + dx, y + dy ) }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        DraggingBoard ->
            drags DragBoard

        DraggingNote noteID ->
            drags (DragNote noteID)

        NotDragging ->
            Sub.none


onMouseDownNoPropagation : msg -> Element.Attribute msg
onMouseDownNoPropagation msg =
    Decode.succeed ( msg, True )
        |> HE.stopPropagationOn "mousedown"
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
