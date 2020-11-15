module ColorVision exposing (Model, Msg(..), init, subscriptions, update, view)

import Array
import Common as C
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n
import List
import Maybe
import Random
import Time
import Tuple



-- MODEL


type Answer
    = Dark
    | Light


type alias Results =
    List ( C.Square, I18n.Result )


type alias Model =
    { time : Int
    , started : Bool
    , hasPlayed : Bool
    , language : I18n.Language
    , possbilities : List C.Square
    , current : C.Square
    , results : Results
    }


initPossibilities : List C.Square
initPossibilities =
    C.allSquares


initModel : Model
initModel =
    { time = 0
    , started = False
    , hasPlayed = False
    , language = I18n.En
    , possbilities = initPossibilities
    , current = C.emptySquare
    , results = []
    }


init : I18n.Language -> Model
init language =
    { initModel | language = language }


isEven : Int -> Bool
isEven x =
    modBy 2 x == 0


isOdd : Int -> Bool
isOdd =
    not << isEven


isDark : C.Square -> Bool
isDark ( x, y ) =
    (isOdd x && isOdd y) || (isEven x && isEven y)


isLight : C.Square -> Bool
isLight =
    not << isDark



-- UPDATE


type Msg
    = NoOp
    | BackHome
    | Roll
    | NewSquareIndex Int
    | Answering Answer
    | Start
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        BackHome ->
            ( model, Cmd.none )

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
                        |> Maybe.withDefault C.emptySquare

                p =
                    C.removeIndexFromList model.possbilities i

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

        Answering answer ->
            let
                success =
                    case answer of
                        Dark ->
                            isDark model.current

                        Light ->
                            isLight model.current
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
    div []
        [ a
            [ onClick BackHome, class "home-btn" ]
            [ text <| I18n.goBackHome model.language ]
        , if model.started then
            viewGame model

          else if model.hasPlayed then
            viewScore model

          else
            viewInit model
        ]


viewInit : Model -> Html Msg
viewInit model =
    div [ class "wrapper" ]
        [ h1 [] [ text <| I18n.colorVision model.language ]
        , p [ class "padded" ]
            [ text <| I18n.colorVisionDescription model.language ]
        , button [ onClick Start ] [ text <| I18n.start model.language ]
        ]


responseMessage : I18n.Language -> C.Square -> String
responseMessage lang square =
    if square == C.emptySquare then
        "\u{00A0}"

    else
        C.squareToString square
            ++ (if isDark square then
                    I18n.isaDarkSquare lang

                else
                    I18n.isaLightSquare lang
               )


getAnimation : Results -> List (Html Msg)
getAnimation results =
    results
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
                        [ text <| C.squareToString square ]
                    ]
            )


viewGame : Model -> Html Msg
viewGame model =
    let
        ( previous, result ) =
            model.results
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( C.emptySquare, I18n.NoResult )
    in
    div [ class "wrapper" ]
        [ h2 [] [ text <| String.fromInt <| model.time // 100 ]
        , div
            [ id "progress-bar"
            , class "wrapper"
            , style "width" <|
                ((model.time - 100)
                    |> toFloat
                    |> C.flip (/) 30
                    |> String.fromFloat
                )
                    ++ "%"
            ]
            []
        , div [ class "square" ]
            [ h1 [] [ text <| C.squareToString model.current ]
            , div [] (getAnimation model.results)
            ]
        , button
            [ id "left", onClick <| Answering Dark ]
            [ text <| I18n.dark model.language ]
        , button
            [ id "right", onClick <| Answering Light ]
            [ text <| I18n.light model.language ]
        , h3
            [ class
                (if result == I18n.Fail then
                    "red"

                 else
                    "green"
                )
            ]
            [ text <| I18n.result model.language result ]
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
