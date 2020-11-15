module Components exposing (..)

import Common as C
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n


progressBar : Int -> List (Html msg)
progressBar time =
    [ h2 []
        [ text <| String.fromInt <| time // 100 ]
    , div
        [ id "progress-bar"
        , class "wrapper"
        , style "width" <|
            ((time - 100)
                |> toFloat
                |> C.flip (/) 30
                |> String.fromFloat
            )
                ++ "%"
        ]
        []
    ]


score : I18n.Language -> Int -> Int -> Html msg
score lang score_ attempt =
    div []
        [ h2 [] [ text <| I18n.score lang ]
        , h1 [ id "score" ]
            [ text <|
                String.fromInt score_
                    ++ "/"
                    ++ String.fromInt attempt
                    ++ " - "
                    ++ String.fromInt (score_ * 100 // attempt)
                    ++ "%"
            ]
        ]


showTimer : I18n.Language -> msg -> Bool -> Html msg
showTimer lang msg isChecked =
    div []
        [ input
            [ type_ "checkbox"
            , name "use-timer"
            , checked isChecked
            , onClick msg
            ]
            []
        , label
            [ for "user-timer"
            , onClick msg
            ]
            [ text <| I18n.useTimer lang ]
        ]


userInput : (String -> msg) -> Html msg
userInput msg =
    (\x -> button [ onClick <| msg x ] [ text x ])
        |> C.flip List.map
            [ "a"
            , "b"
            , "c"
            , "d"
            , "e"
            , "f"
            , "g"
            , "h"
            , "1"
            , "2"
            , "3"
            , "4"
            , "5"
            , "6"
            , "7"
            , "8"
            ]
        |> div [ class "user-input" ]
