module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Keyboard


type alias Model =
    { piece : Piece
    , xy : ( Int, Int )
    , map : Dict ( Int, Int ) Piece
    , keyCode : Keyboard.KeyCode
    }


type Piece
    = Grass
    | Bush
    | Flower
    | Fence


type Msg
    = NoOp
    | Move ( Int, Int )
    | ChangePiece Piece
    | AddPiece


init : ( Model, Cmd Msg )
init =
    ( { piece = Grass
      , xy = ( 0, 0 )
      , map = Dict.empty
      , keyCode = -1
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , viewPieceSelector model
        , viewMap model
        ]


blockStyle : Bool -> List ( String, String )
blockStyle isSelected =
    [ ( "width", "50px" )
    , ( "height", "50px" )
    , ( "border"
      , "1px solid "
            ++ (if isSelected then
                    "red"
                else
                    "black"
               )
      )
    ]


viewPieceSelector : Model -> Html Msg
viewPieceSelector model =
    div []
        [ img
            [ style <| blockStyle (model.piece == Grass)
            , onClick (ChangePiece Grass)
            , src (pieceToSrc Grass)
            ]
            []
        , img
            [ style <| blockStyle (model.piece == Bush)
            , onClick (ChangePiece Bush)
            , src (pieceToSrc Bush)
            ]
            []
        , img
            [ style <| blockStyle (model.piece == Flower)
            , onClick (ChangePiece Flower)
            , src (pieceToSrc Flower)
            ]
            []
        , img
            [ style <| blockStyle (model.piece == Fence)
            , onClick (ChangePiece Fence)
            , src (pieceToSrc Fence)
            ]
            []
        ]


viewMap : Model -> Html Msg
viewMap model =
    div []
        (List.range 0 9
            |> List.map
                (\y ->
                    div
                        [ style
                            [ ( "display", "flex" )
                            ]
                        ]
                        (List.range 0 9
                            |> List.map
                                (\x ->
                                    viewPiece
                                        (Dict.get ( x, y ) model.map)
                                        (model.xy == ( x, y ))
                                )
                        )
                )
        )


viewPiece : Maybe Piece -> Bool -> Html Msg
viewPiece maybePiece isSelected =
    case maybePiece of
        Just piece ->
            let
                color =
                    if isSelected then
                        "cyan"
                    else
                        "red"
            in
                div
                    [ style
                        [ ( "width", "50px" )
                        , ( "height", "50px" )
                        , ( "background", color )
                        , ( "color", color )
                        , ( "border", "1px solid black" )
                        ]
                    ]
                    [ img
                        [ src (pieceToSrc piece)
                        , style
                            [ ( "width", "50px" )
                            , ( "height", "50px" )
                            ]
                        ]
                        []
                    ]

        Nothing ->
            let
                color =
                    if isSelected then
                        "cyan"
                    else
                        "#ccc"
            in
                div
                    [ style
                        [ ( "width", "50px" )
                        , ( "height", "50px" )
                        , ( "background", color )
                        , ( "color", color )
                        , ( "border", "1px solid black" )
                        ]
                    ]
                    [ text "." ]


pieceToSrc : Piece -> String
pieceToSrc piece =
    case piece of
        Grass ->
            "grass.png"

        Bush ->
            "bush.png"

        Flower ->
            "flower.png"

        Fence ->
            "fence.png"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Move xy ->
            ( { model | xy = xy }, Cmd.none )

        ChangePiece piece ->
            ( { model | piece = piece }, Cmd.none )

        AddPiece ->
            let
                map =
                    Dict.insert model.xy model.piece model.map
            in
                ( { model | map = map }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs (handleKeyDowns model.xy)


handleKeyDowns : ( Int, Int ) -> Keyboard.KeyCode -> Msg
handleKeyDowns ( x, y ) keyCode =
    case keyCode of
        37 ->
            -- left
            Move ( x - 1, y )

        38 ->
            -- up
            Move ( x, y - 1 )

        39 ->
            -- right
            Move ( x + 1, y )

        40 ->
            -- down
            Move ( x, y + 1 )

        13 ->
            -- enter
            AddPiece

        _ ->
            NoOp


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
