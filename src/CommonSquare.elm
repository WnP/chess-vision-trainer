module CommonSquare exposing (Model, Msg(..), init, subscriptions, update, view)

import Array
import Browser.Events
import Common as C
import Components
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
    List ( ( C.Position, C.Position ), I18n.Result )


type alias Model =
    { language : I18n.Language
    , time : Int
    , started : Bool
    , hasPlayed : Bool
    , current : ( C.Position, C.Position )
    , possbilities : ( List C.Piece, List C.Square )
    , answers : List String
    , previousAnswers : List String
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
    , current = ( C.emptyPosition, C.emptyPosition )
    , possbilities = initPossibilities
    , answers = []
    , previousAnswers = []
    , results = []
    , buffer = ""
    , useTimer = True
    }


init : I18n.Language -> Model
init language =
    { initModel | language = language }



-- UPDATE


type Direction
    = Left
    | Right


type Msg
    = NoOp
    | BackHome
    | Roll Direction
    | NewPositionIndexes Direction ( Int, Int )
    | Answering String
    | AnsweringNone
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

        Roll direction ->
            ( model
            , Random.pair
                (Random.int 0 (List.length (Tuple.first model.possbilities) - 1))
                (Random.int 0 (List.length (Tuple.second model.possbilities) - 1))
                |> Random.generate (NewPositionIndexes direction)
            )

        NewPositionIndexes direction ( x, y ) ->
            let
                currentPiece =
                    Array.fromList (Tuple.first model.possbilities)
                        |> Array.get x
                        |> Maybe.withDefault C.NoPiece

                currentSquare =
                    Array.fromList (Tuple.second model.possbilities)
                        |> Array.get y
                        |> Maybe.withDefault C.emptySquare

                ps =
                    C.removeIndexFromList (Tuple.second model.possbilities) y

                possbilities =
                    ( C.allPieces
                    , if List.isEmpty ps then
                        C.allSquares

                      else
                        ps
                    )

                current =
                    case direction of
                        Left ->
                            ( ( currentPiece, currentSquare ), C.emptyPosition )

                        Right ->
                            ( Tuple.first model.current, ( currentPiece, currentSquare ) )

                newModel =
                    { model
                        | possbilities = possbilities
                        , current = current
                    }
            in
            case direction of
                Left ->
                    update (Roll Right) newModel

                Right ->
                    ( newModel, Cmd.none )

        Answering char ->
            if not <| String.contains char C.colAndRowChars then
                ( model, Cmd.none )

            else if
                String.length model.buffer
                    == 0
                    && String.contains char C.colChars
            then
                let
                    buffer =
                        char
                in
                ( { model | buffer = buffer }, Cmd.none )

            else if
                String.length model.buffer
                    == 1
                    && String.contains char C.rowChars
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
                check { model | buffer = "", answers = answers } False

            else
                ( model, Cmd.none )

        AnsweringNone ->
            check { model | buffer = "" } True

        Start ->
            update (Roll Left)
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


getSquares : C.Position -> List C.Square
getSquares ( piece, square ) =
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


getSolution : ( C.Position, C.Position ) -> List C.Square
getSolution ( a, b ) =
    getSquares a
        |> List.filter (C.flip List.member <| getSquares b)


check : Model -> Bool -> ( Model, Cmd Msg )
check model isNone =
    let
        answers =
            List.map C.stringToSquare model.answers

        solution =
            getSolution model.current

        status =
            if List.all (C.flip List.member solution) answers then
                if List.length answers == List.length solution then
                    Done

                else if isNone then
                    Fail

                else
                    InProgress

            else
                Fail

        previousAnswers =
            if isNone then
                [ "" ]

            else
                model.answers
    in
    case status of
        Done ->
            update (Roll Left)
                { model
                    | results =
                        List.append
                            model.results
                            [ ( model.current, I18n.Success ) ]
                    , answers = []
                    , previousAnswers = previousAnswers
                }

        Fail ->
            update (Roll Left)
                { model
                    | results =
                        List.append
                            model.results
                            [ ( model.current, I18n.Fail ) ]
                    , answers = []
                    , previousAnswers = previousAnswers
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
        [ h1 [] [ text <| I18n.commonSquares model.language ]
        , p [ class "padded" ]
            [ text <| I18n.commonSquaresDescription model.language ]
        , Components.showTimer model.language ToggleTimer model.useTimer
        , button [ onClick Start ] [ text <| I18n.start model.language ]
        ]


currentToString : I18n.Language -> ( C.Position, C.Position ) -> String
currentToString lang ( left, right ) =
    I18n.positionToString lang left
        ++ " - "
        ++ I18n.positionToString lang right


getAnimation : I18n.Language -> Results -> List (Html Msg)
getAnimation lang results =
    results
        |> List.map
            (\( current, result ) ->
                div [ class "result-wrapper" ]
                    [ h1
                        [ class <|
                            case result of
                                I18n.Success ->
                                    "success"

                                _ ->
                                    "fail"
                        ]
                        [ currentToString lang current
                            |> text
                        ]
                    ]
            )


viewGame : Model -> Html Msg
viewGame model =
    let
        ( previous, result ) =
            model.results
                |> List.reverse
                |> List.head
                |> Maybe.withDefault
                    ( ( C.emptyPosition, C.emptyPosition )
                    , I18n.NoResult
                    )
    in
    div [ class "wrapper" ]
        [ div []
            (if model.useTimer then
                Components.progressBar model.time

             else
                []
            )
        , div [ class "square" ]
            [ h1 [] [ text <| currentToString model.language model.current ]
            , div [] (getAnimation model.language model.results)
            ]
        , Components.userInput Answering
        , button
            [ id "none-btn", onClick AnsweringNone ]
            [ text <| I18n.none model.language ]
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
            (let
                solution =
                    getSolution previous
                        |> formatSolution model
             in
             if List.isEmpty solution then
                [ text "\u{00A0}" ]

             else
                solution
            )
        ]


formatSolution : Model -> List C.Square -> List (Html Msg)
formatSolution model solution =
    let
        answers =
            model.previousAnswers
                |> List.filter ((/=) "")
                |> List.map C.stringToSquare
    in
    ((solution
        |> List.map
            (\s ->
                if List.member s answers then
                    span [ class "green" ] [ text <| C.squareToString s ]

                else
                    span [] [ text <| C.squareToString s ]
            )
     )
        ++ (answers
                |> List.filter (not << C.flip List.member solution)
                |> List.map
                    (\s ->
                        span [ class "red" ] [ text <| C.squareToString s ]
                    )
           )
    )
        |> List.intersperse (text ", ")


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
        [ Components.score model.language score <| List.length model.results
        , Components.showTimer model.language ToggleTimer model.useTimer
        , button [ onClick Start ] [ text <| I18n.restart model.language ]
        ]
