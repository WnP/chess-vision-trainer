module Home exposing
    ( Game(..)
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n



-- MODEL


type alias Model =
    { language : I18n.Language }


initModel : Model
initModel =
    { language = I18n.En }


init : I18n.Language -> Model
init language =
    { initModel | language = language }



-- UPDATE


type Game
    = ColorVision
    | PieceMove


type Msg
    = NoOp
    | Play Game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Play ColorVision ->
            ( model, Cmd.none )

        Play PieceMove ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ h1 [] [ text "Chess Vision Trainer" ]
        , p [ class "padded" ] [ text <| I18n.description model.language ]
        , div [ id "menu" ]
            [ button
                [ onClick <| Play ColorVision ]
                [ text <| I18n.colorVision model.language ]
            , button
                [ onClick <| Play PieceMove ]
                [ text <| I18n.pieceMoves model.language ]
            ]
        ]
