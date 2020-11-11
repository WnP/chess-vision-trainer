module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    , result : String
    , response : String
    , time : Int
    , started : Bool
    , score : Int
    , attempt : Int
    , hasPlayed : Bool
    }


initModel : Model
initModel =
    { square = ( 0, 0 )
    , result = "\u{00A0}"
    , response = "\u{00A0}"
    , time = 0
    , started = False
    , score = 0
    , attempt = 0
    , hasPlayed = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
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


getResponse : ( Int, Int ) -> String
getResponse square =
    toString square
        ++ " is a "
        ++ (if isDark square then
                "Dark"

            else
                "Light"
           )
        ++ " square."



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
                "Success"

            else
                "Fail"
        , response = getResponse model.square
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
        , p [ class "padded" ]
            [ text <|
                "Once started you should guess if the displayed square "
                    ++ "is a Light or a Dark one. This excercice may improve "
                    ++ "your chess vision."
            ]
        , button [ onClick Start ] [ text "Start" ]
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
        , button [ onClick Dark ] [ text "Dark" ]
        , button [ onClick Light ] [ text "Light" ]
        , h3
            [ class
                (if model.result == "Fail" then
                    "red"

                 else
                    "green"
                )
            ]
            [ text model.result ]
        , p [] [ text model.response ]
        ]


viewScore : Model -> Html Msg
viewScore model =
    div []
        [ h2 [] [ text "Your score is: " ]
        , h1 []
            [ text <|
                String.fromInt model.score
                    ++ "/"
                    ++ String.fromInt model.attempt
                    ++ " - "
                    ++ String.fromInt (model.score * 100 // model.attempt)
                    ++ "%"
            ]
        , button [ onClick Start ] [ text "Restart" ]
        ]
