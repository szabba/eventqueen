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
    , contextMenu : ContextMenu
    }


type Msg
    = NoOp
    | StartBoardDrag
    | DragBoard ( Float, Float )
    | StartNoteDrag Int
    | DragNote Int ( Float, Float )
    | StopDrag
    | OpenMenu ContextMenu


type NodeID
    = NodeID String


type DragState
    = NotDragging
    | DraggingBoard
    | DraggingNote Int


type ContextMenu
    = NoMenu
    | BoardMenu ( Float, Float )
    | CardMenu Int ( Float, Float )


type alias Note =
    { offset : ( Float, Float )
    , text : String
    }


type alias ViewConfig msg =
    { onContextMenu : (( Float, Float ) -> msg) -> Element.Attribute msg }


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
      , contextMenu = NoMenu
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    let
        viewConfig =
            model |> viewConfigOf

        floats =
            [ model.notes |> viewNotes viewConfig
            , model.contextMenu
                |> viewContextMenu
                |> List.singleton
            ]
                |> List.concat
                |> List.map Element.inFront
    in
    { title = "Event Queen"
    , body =
        onBoard viewConfig model <| Element.el floats Element.none
    }


viewConfigOf : Model -> ViewConfig Msg
viewConfigOf { offset } =
    let
        ( offX, offY ) =
            offset

        fixOffset ( x, y ) =
            ( x - offX, y - offY )
    in
    { onContextMenu = \toMsg -> (fixOffset >> toMsg) |> onContextMenu }


viewNotes : ViewConfig Msg -> Dict Int Note -> List (Element Msg)
viewNotes config notes =
    notes
        |> Dict.toList
        |> List.map (viewNote config)


viewContextMenu : ContextMenu -> Element Msg
viewContextMenu menu =
    case menu of
        NoMenu ->
            Element.none

        BoardMenu offset ->
            boardMenu
                |> viewMenu
                |> withOffset offset

        CardMenu noteID offset ->
            cardMenu noteID
                |> viewMenu
                |> withOffset offset


viewNote : ViewConfig Msg -> ( Int, Note ) -> Element Msg
viewNote config ( noteID, { offset, text } ) =
    withOffset offset <|
        Element.el
            [ Border.color noteBorder
            , Border.width 1
            , Background.color noteBackground
            , Element.padding 20
            , Element.width (Element.px 200)
            , Element.height (Element.px 200)
            , onMouseDownNoPropagation (StartNoteDrag noteID)
            , config.onContextMenu (OpenMenu << CardMenu noteID)
            , Element.htmlAttribute <| HA.style "user-select" "none"
            , Element.htmlAttribute <| HA.style "-moz-user-select" "none"
            , Element.htmlAttribute <| HA.style "-webkit-user-select" "none"
            , Element.htmlAttribute <| HA.style "-ms-user-select" "none"
            ]
        <|
            Element.paragraph [ Element.height Element.fill ]
                [ Element.text text
                ]


boardMenu : List ( String, Msg )
boardMenu =
    [ "New card"
    , "Fit all"
    ]
        |> List.map (\name -> ( name, NoOp ))


cardMenu : Int -> List ( String, Msg )
cardMenu noteID =
    [ "Edit"
    , "Remove"
    ]
        |> List.map (\name -> ( name, NoOp ))


viewMenu : List ( String, msg ) -> Element msg
viewMenu rows =
    rows
        |> List.map viewMenuRow
        |> Element.column
            [ Border.width 1
            , Border.color noteBorder
            , Background.color board
            , Element.padding 15
            , Element.spacing 15
            ]


viewMenuRow : ( String, msg ) -> Element msg
viewMenuRow ( name, msg ) =
    Element.row
        [ Element.width Element.fill
        , Events.onClick msg
        ]
        [ Element.text name ]


onBoard : ViewConfig Msg -> { a | offset : ( Float, Float ) } -> Element Msg -> List (Html Msg)
onBoard config ({ offset } as model) =
    let
        ( dx, dy ) =
            offset
    in
    List.singleton
        << Element.layout
            [ Element.clip
            , Element.width Element.fill
            , Element.height Element.fill
            , Events.onMouseDown StartBoardDrag
            , Events.onMouseUp StopDrag
            , Background.color board
            , config.onContextMenu (OpenMenu << BoardMenu)
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
            ( { model | dragState = DraggingBoard, contextMenu = NoMenu }
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
            ( { model | dragState = DraggingNote noteID, contextMenu = NoMenu }
            , Cmd.none
            )

        DragNote noteID offsetDelta ->
            ( { model | notes = model.notes |> Dict.update noteID (Maybe.map <| updateOffsetBy offsetDelta) }
            , Cmd.none
            )

        -- TODO: This should come in board-relative coordinates
        OpenMenu newMenu ->
            ( { model | contextMenu = newMenu }
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


onContextMenu : (( Float, Float ) -> msg) -> Element.Attribute msg
onContextMenu toMsg =
    mouseAt
        |> Decode.map
            (\at ->
                { message = at |> toMsg
                , preventDefault = True
                , stopPropagation = True
                }
            )
        |> HE.custom "contextmenu"
        |> Element.htmlAttribute


clientToBoard : { boardOffset : ( Float, Float ), pos : ( Float, Float ) } -> ( Float, Float )
clientToBoard { boardOffset, pos } =
    let
        ( ( offX, offY ), ( posX, posY ) ) =
            ( boardOffset, pos )
    in
    ( posX - offX, posY - offY )


onMouseDownNoPropagation : msg -> Element.Attribute msg
onMouseDownNoPropagation msg =
    ( msg, True )
        |> Decode.succeed
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
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
