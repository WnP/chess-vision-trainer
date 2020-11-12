module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n
import Random
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


type alias Model =
    { square : ( Int, Int )
    , result : I18n.Result
    , response : String
    , time : Int
    , started : Bool
    , score : Int
    , attempt : Int
    , hasPlayed : Bool
    , language : I18n.Language
    }


initModel : Model
initModel =
    { square = ( 0, 0 )
    , result = I18n.NoResult
    , response = "\u{00A0}"
    , time = 0
    , started = False
    , score = 0
    , attempt = 0
    , hasPlayed = False
    , language = I18n.En
    }


init : String -> ( Model, Cmd Msg )
init language =
    ( { initModel | language = I18n.parseLang language }
    , Cmd.none
    )


toString : ( Int, Int ) -> String
toString ( x, y ) =
    if x == 0 then
        ""

    else
        String.fromChar (Char.fromCode <| x + 96) ++ String.fromInt y


isEven : Int -> Bool
isEven x =
    modBy 2 x == 0


isOdd : Int -> Bool
isOdd =
    not << isEven


isDark : ( Int, Int ) -> Bool
isDark ( x, y ) =
    (isOdd x && isOdd y) || (isEven x && isEven y)


getResponse : Model -> String
getResponse model =
    toString model.square
        ++ (if isDark model.square then
                I18n.isaDarkSquare model.language

            else
                I18n.isaDarkSquare model.language
           )



-- UPDATE


type Msg
    = Roll
    | NewSquare ( Int, Int )
    | Dark
    | Light
    | Start
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewSquare <|
                Random.pair (Random.int 1 8) (Random.int 1 8)
            )

        NewSquare square ->
            if square == model.square then
                update Roll model

            else
                ( { model | square = square }
                , Cmd.none
                )

        Dark ->
            let
                success =
                    isDark model.square
            in
            update Roll <| updateModel model success

        Light ->
            let
                success =
                    not <| isDark model.square
            in
            update Roll <| updateModel model success

        Start ->
            update Roll
                { initModel
                    | time = 3100
                    , started = True
                    , hasPlayed = True
                    , language = model.language
                }

        Tick _ ->
            let
                newTime =
                    model.time - 1
            in
            if model.started && newTime == 100 then
                ( { model | time = newTime, started = False }
                , Cmd.none
                )

            else if model.started then
                ( { model | time = newTime }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


updateModel : Model -> Bool -> Model
updateModel model success =
    { model
        | result =
            if success then
                I18n.Success

            else
                I18n.Fail
        , response = getResponse model
        , score =
            model.score
                + (if success then
                    1

                   else
                    0
                  )
        , attempt = model.attempt + 1
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 10 Tick



-- VIEW


view : Model -> Html Msg
view model =
    if model.started then
        viewGame model

    else if model.hasPlayed then
        viewScore model

    else
        viewInit model


viewInit : Model -> Html Msg
viewInit model =
    div []
        [ h1 [] [ text "Chess Vision Trainer" ]
        , p [ class "padded" ] [ text <| I18n.description model.language ]
        , button [ onClick Start ] [ text <| I18n.start model.language ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    div []
        [ h2 [] [ text <| String.fromInt <| model.time // 100 ]
        , div
            [ id "progress-bar"
            , style "width" <|
                (String.fromFloat <|
                    (toFloat <| (model.time - 10) * 100)
                        / 3000
                )
                    ++ "%"
            ]
            []
        , h1 [] [ text <| toString model.square ]
        , button [ onClick Dark ] [ text <| I18n.dark model.language ]
        , button [ onClick Light ] [ text <| I18n.light model.language ]
        , h3
            [ class
                (if model.result == I18n.Fail then
                    "red"

                 else
                    "green"
                )
            ]
            [ text <| I18n.resultToString model.result model.language ]
        , p [] [ text model.response ]
        ]


viewScore : Model -> Html Msg
viewScore model =
    div []
        [ h2 [] [ text <| I18n.score model.language ]
        , h1 []
            [ text <|
                String.fromInt model.score
                    ++ "/"
                    ++ String.fromInt model.attempt
                    ++ " - "
                    ++ String.fromInt (model.score * 100 // model.attempt)
                    ++ "%"
            ]
        , button [ onClick Start ] [ text <| I18n.restart model.language ]
        ]
