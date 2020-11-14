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


type Answer
    = Dark
    | Light


type alias Results =
    List ( Square, I18n.Result )


type alias Model =
    { time : Int
    , started : Bool
    , hasPlayed : Bool
    , language : I18n.Language
    , possbilities : List Square
    , current : Square
    , results : Results
    }


initPossibilities : List Square
initPossibilities =
    List.range 1 8
        |> List.concatMap (Tuple.pair >> flip List.map (List.range 1 8))


initModel : Model
initModel =
    { time = 0
    , started = False
    , hasPlayed = False
    , language = I18n.En
    , possbilities = initPossibilities
    , current = emptySquare
    , results = []
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
    | Answering Answer Square
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
                | possbilities = possbilities
                , current = current
              }
            , Cmd.none
            )

        Answering answer square ->
            let
                success =
                    case answer of
                        Dark ->
                            isDark square

                        Light ->
                            isLight square
            in
            update Roll
                { model
                    | results =
                        List.append model.results
                            [ ( model.current
                              , if success then
                                    I18n.Success

                                else
                                    I18n.Fail
                              )
                            ]
                }

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
                ( { model | started = False }
                , Cmd.none
                )

            else if model.started then
                ( { model | time = newTime }
                , Cmd.none
                )

            else
                ( model, Cmd.none )



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
    div [ class "wrapper" ]
        [ h1 [] [ text "Chess Vision Trainer" ]
        , p [ class "padded" ] [ text <| I18n.description model.language ]
        , button [ onClick Start ] [ text <| I18n.start model.language ]
        ]


responseMessage : I18n.Language -> Square -> String
responseMessage lang square =
    if square == emptySquare then
        "\u{00A0}"

    else
        squareToString square
            ++ (if isDark square then
                    I18n.isaDarkSquare lang

                else
                    I18n.isaLightSquare lang
               )


getAnimation : Model -> List (Html Msg)
getAnimation model =
    model.results
        |> List.map
            (\( square, result ) ->
                div [ class "result-wrapper" ]
                    [ h1
                        [ class <|
                            case result of
                                I18n.Success ->
                                    "success"

                                _ ->
                                    "fail"
                        ]
                        [ text <| squareToString square ]
                    ]
            )


viewGame : Model -> Html Msg
viewGame model =
    let
        ( previous, result ) =
            model.results
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( emptySquare, I18n.NoResult )
    in
    div [ class "wrapper" ]
        [ h2 [] [ text <| String.fromInt <| model.time // 100 ]
        , div
            [ id "progress-bar"
            , class "wrapper"
            , style "width" <|
                ((model.time - 100)
                    |> toFloat
                    |> flip (/) 30
                    |> String.fromFloat
                )
                    ++ "%"
            ]
            []
        , div [ class "square" ]
            [ h1 [] [ text <| squareToString model.current ]
            , div [] (getAnimation model)
            ]
        , button
            [ id "left", onClick <| Answering Dark model.current ]
            [ text <| I18n.dark model.language ]
        , button
            [ id "right", onClick <| Answering Light model.current ]
            [ text <| I18n.light model.language ]
        , h3
            [ class
                (if result == I18n.Fail then
                    "red"

                 else
                    "green"
                )
            ]
            [ text <| I18n.resultToString result model.language ]
        , p [] [ text <| responseMessage model.language previous ]
        ]


viewScore : Model -> Html Msg
viewScore model =
    let
        score =
            model.results
                |> List.map Tuple.second
                |> List.filter ((==) I18n.Success)
                |> List.length
    in
    div [ class "wrapper" ]
        [ h2 [] [ text <| I18n.score model.language ]
        , h1 [ id "score" ]
            [ text <|
                String.fromInt score
                    ++ "/"
                    ++ String.fromInt (List.length model.results)
                    ++ " - "
                    ++ String.fromInt (score * 100 // List.length model.results)
                    ++ "%"
            ]
        , button [ onClick Start ] [ text <| I18n.restart model.language ]
        ]
