module PieceMove exposing (Model, Msg(..), init, subscriptions, update, view)

import Array
import Browser.Events
import Common as C
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n
import Json.Decode as Json
import List
import Maybe
import Random
import Time



-- MODEL


type alias Results =
    List ( C.Position, I18n.Result )


type alias Model =
    { language : I18n.Language
    , time : Int
    , started : Bool
    , hasPlayed : Bool
    , current : C.Position
    , possbilities : ( List C.Piece, List C.Square )
    , answers : List String
    , results : Results
    , buffer : String
    , useTimer : Bool
    }


initPossibilities : ( List C.Piece, List C.Square )
initPossibilities =
    ( C.allPieces, C.allSquares )


initModel : Model
initModel =
    { language = I18n.En
    , time = 0
    , started = False
    , hasPlayed = False
    , current = C.emptyPosition
    , possbilities = initPossibilities
    , answers = []
    , results = []
    , buffer = ""
    , useTimer = True
    }


init : I18n.Language -> Model
init language =
    { initModel | language = language }


rowChars : String
rowChars =
    "12345678"


colChars : String
colChars =
    "abcdefgh"


colAndRowChars : String
colAndRowChars =
    colChars ++ rowChars


stringToSquare : String -> C.Square
stringToSquare s =
    ( String.left 1 s
        |> C.flip String.indexes colChars
        |> List.head
        |> Maybe.withDefault 0
        |> (+) 1
    , String.right 1 s
        |> C.flip String.indexes rowChars
        |> List.head
        |> Maybe.withDefault 0
        |> (+) 1
    )



-- UPDATE


type Msg
    = NoOp
    | BackHome
    | Roll
    | NewPositionIndexes ( Int, Int )
    | Answering String
    | Start
    | Tick Time.Posix
    | Keypress String
    | ToggleTimer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        BackHome ->
            ( model, Cmd.none )

        Roll ->
            ( model
            , Random.pair
                (Random.int 0 (List.length (Tuple.first model.possbilities) - 1))
                (Random.int 0 (List.length (Tuple.second model.possbilities) - 1))
                |> Random.generate NewPositionIndexes
            )

        NewPositionIndexes ( x, y ) ->
            let
                currentPiece =
                    Array.fromList (Tuple.first model.possbilities)
                        |> Array.get x
                        |> Maybe.withDefault C.NoPiece

                currentSquare =
                    Array.fromList (Tuple.second model.possbilities)
                        |> Array.get y
                        |> Maybe.withDefault C.emptySquare

                pp =
                    C.removeIndexFromList (Tuple.first model.possbilities) x

                ps =
                    C.removeIndexFromList (Tuple.second model.possbilities) y

                possbilities =
                    ( if List.isEmpty pp then
                        C.allPieces

                      else
                        pp
                    , if List.isEmpty ps then
                        C.allSquares

                      else
                        ps
                    )
            in
            ( { model
                | possbilities = possbilities
                , current = ( currentPiece, currentSquare )
              }
            , Cmd.none
            )

        Answering char ->
            if not <| String.contains char colAndRowChars then
                ( model, Cmd.none )

            else if
                String.length model.buffer
                    == 0
                    && String.contains char colChars
            then
                let
                    buffer =
                        char
                in
                ( { model | buffer = buffer }, Cmd.none )

            else if
                String.length model.buffer
                    == 1
                    && String.contains char rowChars
            then
                let
                    buffer =
                        model.buffer ++ char

                    answers =
                        -- avoid duplicate answers
                        if List.member buffer model.answers then
                            model.answers

                        else
                            List.append model.answers [ buffer ]
                in
                check { model | buffer = "", answers = answers }

            else
                ( model, Cmd.none )

        Start ->
            update Roll
                { initModel
                    | time = 3100
                    , started = True
                    , hasPlayed = True
                    , language = model.language
                    , useTimer = model.useTimer
                }

        Tick _ ->
            let
                newTime =
                    model.time - 1
            in
            if model.started && newTime == 100 && model.useTimer then
                ( { model | started = False }
                , Cmd.none
                )

            else if model.started then
                ( { model | time = newTime }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Keypress char ->
            if model.started then
                update (Answering char) model

            else
                ( model, Cmd.none )

        ToggleTimer ->
            ( { model | useTimer = not model.useTimer }, Cmd.none )


type Status
    = InProgress
    | Done
    | Fail


getSolution : C.Position -> List C.Square
getSolution ( piece, square ) =
    case piece of
        C.NoPiece ->
            []

        C.Queen ->
            C.queenMoves square

        C.Knight ->
            C.knightMoves square

        C.Rook ->
            C.rookMoves square

        C.Bishop ->
            C.bishopMoves square


check : Model -> ( Model, Cmd Msg )
check model =
    let
        answers =
            List.map stringToSquare model.answers

        solution =
            getSolution model.current

        status =
            if List.all (C.flip List.member solution) answers then
                if List.length answers == List.length solution then
                    Done

                else
                    InProgress

            else
                Fail
    in
    case status of
        Done ->
            update Roll
                { model
                    | results =
                        List.append
                            model.results
                            [ ( model.current, I18n.Success ) ]
                    , answers = []
                }

        Fail ->
            update Roll
                { model
                    | results =
                        List.append
                            model.results
                            [ ( model.current, I18n.Fail ) ]
                    , answers = []
                }

        InProgress ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


decodeKey : Json.Decoder Msg
decodeKey =
    Json.field "key" (Json.string |> Json.map Keypress)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 10 Tick
        , Browser.Events.onKeyPress decodeKey
        ]



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
        [ h1 [] [ text <| I18n.pieceMoves model.language ]
        , p [ class "padded" ]
            [ text <| I18n.pieceMoveDescription model.language ]
        , div []
            [ input
                [ type_ "checkbox"
                , name "use-timer"
                , checked model.useTimer
                , onClick ToggleTimer
                ]
                []
            , label
                [ for "user-timer"
                , onClick ToggleTimer
                ]
                [ text <| I18n.useTimer model.language ]
            ]
        , button [ onClick Start ] [ text <| I18n.start model.language ]
        ]


getAnimation : I18n.Language -> Results -> List (Html Msg)
getAnimation lang results =
    results
        |> List.map
            (\( position, result ) ->
                div [ class "result-wrapper" ]
                    [ h1
                        [ class <|
                            case result of
                                I18n.Success ->
                                    "success"

                                _ ->
                                    "fail"
                        ]
                        [ text <| I18n.positionToString lang position ]
                    ]
            )


viewGame : Model -> Html Msg
viewGame model =
    let
        ( previous, result ) =
            model.results
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( C.emptyPosition, I18n.NoResult )
    in
    div [ class "wrapper" ]
        [ div []
            (if model.useTimer then
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
                ]

             else
                []
            )
        , div [ class "square" ]
            [ h1 [] [ text <| I18n.positionToString model.language model.current ]
            , div [] (getAnimation model.language model.results)
            ]
        , (\x -> button [ onClick <| Answering x ] [ text x ])
            |> C.flip List.map [ "a", "b", "c", "d", "e", "f", "g", "h", "1", "2", "3", "4", "5", "6", "7", "8" ]
            |> div [ class "user-input" ]
        , p []
            [ model.answers
                |> C.flip List.append
                    (if String.length model.buffer == 1 then
                        [ model.buffer ]

                     else
                        []
                    )
                |> String.join ", "
                |> (++) "\u{00A0}"
                |> text
            ]
        , h3
            [ class
                (if result == I18n.Fail then
                    "red"

                 else
                    "green"
                )
            ]
            [ text <| I18n.result model.language result ]
        , p []
            [ getSolution previous
                |> List.map C.squareToString
                |> String.join ", "
                |> (++) "\u{00A0}"
                |> text
            ]
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
