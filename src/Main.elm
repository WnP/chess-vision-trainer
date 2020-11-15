module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import ColorVision as ColorVision
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n
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
    = ColorVision ColorVision.Model


init : String -> ( Model, Cmd Msg )
init language =
    ( ColorVision (ColorVision.init language)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | ColorVisionMsg ColorVision.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ColorVisionMsg colorVisionMsg, ColorVision colorVisionModel ) ->
            ColorVision.update colorVisionMsg colorVisionModel
                |> updateWith ColorVision ColorVisionMsg model

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



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        ColorVision colorVisionModel ->
            Html.map ColorVisionMsg <| ColorVision.view colorVisionModel
