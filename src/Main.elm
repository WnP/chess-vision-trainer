module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import ColorVision
import Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n
import PieceMove
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Home Home.Model
    | ColorVision ColorVision.Model
    | PieceMove PieceMove.Model


init : String -> ( Model, Cmd Msg )
init language =
    ( Home (Home.init <| I18n.parseLang language)
    , Cmd.none
    )



-- UPDATE


type Msg
    = HomeMsg Home.Msg
    | ColorVisionMsg ColorVision.Msg
    | PieceMoveMsg PieceMove.Msg


backHome : Model -> I18n.Language -> ( Model, Cmd Msg )
backHome model lang =
    Home.init lang
        |> Home.update Home.NoOp
        |> updateWith Home HomeMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( HomeMsg (Home.Play Home.ColorVision), Home homeModel ) ->
            ColorVision.init homeModel.language
                |> ColorVision.update ColorVision.NoOp
                |> updateWith ColorVision ColorVisionMsg model

        ( HomeMsg (Home.Play Home.PieceMove), Home homeModel ) ->
            PieceMove.init homeModel.language
                |> PieceMove.update PieceMove.NoOp
                |> updateWith PieceMove PieceMoveMsg model

        ( ColorVisionMsg ColorVision.BackHome, ColorVision colorVisionModel ) ->
            backHome model colorVisionModel.language

        ( ColorVisionMsg colorVisionMsg, ColorVision colorVisionModel ) ->
            ColorVision.update colorVisionMsg colorVisionModel
                |> updateWith ColorVision ColorVisionMsg model

        ( PieceMoveMsg PieceMove.BackHome, PieceMove pieceMoveModel ) ->
            backHome model pieceMoveModel.language

        ( PieceMoveMsg pieceMoveMsg, PieceMove pieceMoveModel ) ->
            PieceMove.update pieceMoveMsg pieceMoveModel
                |> updateWith PieceMove PieceMoveMsg model

        _ ->
            ( model, Cmd.none )


updateWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ColorVision colorVisionModel ->
            Sub.map ColorVisionMsg (ColorVision.subscriptions colorVisionModel)

        PieceMove pieceMoveModel ->
            Sub.map PieceMoveMsg (PieceMove.subscriptions pieceMoveModel)

        Home homeModel ->
            Sub.map HomeMsg (Home.subscriptions homeModel)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        ColorVision colorVisionModel ->
            Html.map ColorVisionMsg <| ColorVision.view colorVisionModel

        PieceMove pieceMoveModel ->
            Html.map PieceMoveMsg <| PieceMove.view pieceMoveModel

        Home homeModel ->
            Html.map HomeMsg <| Home.view homeModel
