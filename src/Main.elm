module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array
import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n
import List
import Maybe
import Random
import Time
import Tuple



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Square =
    ( Int, Int )


emptySquare : Square
emptySquare =
    ( 0, 0 )


type alias Model =
    { result : I18n.Result
    , time : Int
    , started : Bool
    , score : Int
    , attempt : Int
    , hasPlayed : Bool
    , language : I18n.Language
    , possbilities : List Square
    , previous : Square
    , current : Square
    }


initPossibilities : List Square
initPossibilities =
    List.range 1 8
        |> List.concatMap (Tuple.pair >> flip List.map (List.range 1 8))


initModel : Model
initModel =
    { result = I18n.NoResult
    , time = 0
    , started = False
    , score = 0
    , attempt = 0
    , hasPlayed = False
    , language = I18n.En
    , possbilities = initPossibilities
    , previous = emptySquare
    , current = emptySquare
    }


init : String -> ( Model, Cmd Msg )
init language =
    ( { initModel | language = I18n.parseLang language }
    , Cmd.none
    )


squareToString : Square -> String
squareToString ( x, y ) =
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


flip : (a -> b -> c) -> b -> a -> c
flip func b a =
    func a b


isDark : Square -> Bool
isDark ( x, y ) =
    (isOdd x && isOdd y) || (isEven x && isEven y)


isLight : Square -> Bool
isLight =
    not << isDark



-- UPDATE


type Msg
    = Roll
    | NewSquareIndex Int
    | Dark Square
    | Light Square
    | Start
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.int 0 (List.length model.possbilities - 1)
                |> Random.generate NewSquareIndex
            )

        NewSquareIndex i ->
            let
                current =
                    Array.fromList model.possbilities
                        |> Array.get i
                        |> Maybe.withDefault emptySquare

                p =
                    List.drop (i + 1) model.possbilities
                        |> List.append (List.take i model.possbilities)

                possbilities =
                    if List.isEmpty p then
                        initPossibilities

                    else
                        p
            in
            ( { model
                | previous = model.current
                , possbilities = possbilities
                , current = current
              }
            , Cmd.none
            )

        Dark square ->
            isDark square
                |> updateModel model
                |> update Roll

        Light square ->
            isLight square
                |> updateModel model
                |> update Roll

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


responseMessage : Model -> String
responseMessage model =
    if model.previous == emptySquare then
        "\u{00A0}"

    else
        squareToString model.previous
            ++ (if isDark model.previous then
                    I18n.isaDarkSquare model.language

                else
                    I18n.isaLightSquare model.language
               )


viewGame : Model -> Html Msg
viewGame model =
    div []
        [ h2 [] [ text <| String.fromInt <| model.time // 100 ]
        , div
            [ id "progress-bar"
            , style "width" <|
                ((model.time - 100)
                    |> toFloat
                    |> flip (/) 30
                    |> String.fromFloat
                )
                    ++ "%"
            ]
            []
        , h1 [] [ text <| squareToString model.current ]
        , button
            [ id "left", onClick <| Dark model.current ]
            [ text <| I18n.dark model.language ]
        , button
            [ id "right", onClick <| Light model.current ]
            [ text <| I18n.light model.language ]
        , h3
            [ class
                (if model.result == I18n.Fail then
                    "red"

                 else
                    "green"
                )
            ]
            [ text <| I18n.resultToString model.result model.language ]
        , p [] [ text <| responseMessage model ]
        ]


viewScore : Model -> Html Msg
viewScore model =
    div []
        [ h2 [] [ text <| I18n.score model.language ]
        , h1 [ id "score" ]
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
