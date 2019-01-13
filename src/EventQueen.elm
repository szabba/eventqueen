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
import EventQueen.Card as Card exposing (Card)
import EventQueen.MultivalueRegister as MVR
import EventQueen.Node as Node exposing (Node)
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
    , viewModel : ViewModel
    , node : Node Card.Diff Card
    }


type alias ViewModel =
    { offset : ( Float, Float )
    , dragState : DragState
    , contextMenu : ContextMenu
    }


type Msg
    = NoOp
    | StartBoardDrag
    | DragBoard ( Float, Float )
    | StartCardDrag Int
    | DragCard Int ( Float, Float )
    | StopDrag
    | OpenMenu ContextMenu


type NodeID
    = NodeID String


type DragState
    = NotDragging
    | DraggingBoard
    | DraggingCard Int


type ContextMenu
    = NoMenu
    | BoardMenu ( Float, Float )
    | CardMenu Int ( Float, Float )


type alias Note =
    { offset : ( Float, Float )
    , text : String
    }


type alias ViewConfig =
    { clientToBoard : ( Float, Float ) -> ( Float, Float )
    }


init : Flags -> ( Model, Cmd Msg )
init { rawNodeID } =
    ( { nodeID = NodeID rawNodeID
      , viewModel =
            { offset = ( 0, 0 )
            , dragState = NotDragging
            , contextMenu = NoMenu
            }
      , node = Node.init Card.config { name = "node" }
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    let
        viewConfig =
            model |> viewConfigOf

        floats =
            [ Dict.empty |> Dict.insert 0 model.node.state |> viewCards viewConfig
            , model.viewModel.contextMenu
                |> viewContextMenu
                |> List.singleton
            ]
                |> List.concat
                |> List.map Element.inFront
    in
    { title = "Event Queen"
    , body =
        onBoard viewConfig model.viewModel <| Element.el floats Element.none
    }


viewConfigOf : Model -> ViewConfig
viewConfigOf { viewModel } =
    let
        ( offX, offY ) =
            viewModel.offset

        clientToBoard ( x, y ) =
            ( x - offX, y - offY )
    in
    { clientToBoard = clientToBoard
    }


viewCards : ViewConfig -> Dict Int Card -> List (Element Msg)
viewCards config cards =
    cards
        |> Dict.toList
        |> List.map (viewCard config)


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


viewCard : ViewConfig -> ( Int, Card ) -> Element Msg
viewCard config ( noteID, card ) =
    let
        offset =
            card.position
                |> MVR.get
                |> List.head
                |> Maybe.withDefault ( 0, 0 )

        text =
            card.text
                |> MVR.get
                |> List.head
                |> Maybe.withDefault "..."
    in
    withOffset offset <|
        Element.el
            [ Border.color noteBorder
            , Border.width 1
            , Background.color noteBackground
            , Element.padding 20
            , Element.width (Element.px 200)
            , Element.height (Element.px 200)
            , onMouseDownNoPropagation (StartCardDrag noteID)
            , onContextMenu (config.clientToBoard >> CardMenu noteID >> OpenMenu)
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


onBoard : ViewConfig -> ViewModel -> Element Msg -> List (Html Msg)
onBoard config ({ offset } as viewModel) =
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
            , onContextMenu (config.clientToBoard >> BoardMenu >> OpenMenu)
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
update msg ({ viewModel } as model) =
    case msg of
        NoOp ->
            model |> noCmd

        StopDrag ->
            model
                |> setViewModel { viewModel | dragState = NotDragging }
                |> noCmd

        StartBoardDrag ->
            model
                |> setViewModel { viewModel | dragState = DraggingBoard, contextMenu = NoMenu }
                |> noCmd

        DragBoard ( dx, dy ) ->
            let
                ( x, y ) =
                    viewModel.offset
            in
            model
                |> setViewModel { viewModel | offset = ( x + dx, y + dy ) }
                |> noCmd

        StartCardDrag noteID ->
            model
                |> setViewModel { viewModel | dragState = DraggingCard noteID, contextMenu = NoMenu }
                |> noCmd

        DragCard noteID offset ->
            { model | node = model.node |> Node.update Card.config (Card.moveBy offset) }
                |> noCmd

        OpenMenu newMenu ->
            model
                |> setViewModel { viewModel | contextMenu = newMenu }
                |> noCmd


setViewModel : ViewModel -> Model -> Model
setViewModel viewModel model =
    { model | viewModel = viewModel }


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { viewModel } =
    case viewModel.dragState of
        DraggingBoard ->
            drags DragBoard

        DraggingCard noteID ->
            drags (DragCard noteID)

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
