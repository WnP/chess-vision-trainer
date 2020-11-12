module I18n exposing (..)


type Language
    = En
    | Fr


parseLang : String -> Language
parseLang l =
    let
        lang =
            String.toLower l
    in
    if String.contains "fr" lang then
        Fr

    else
        En


type Result
    = Success
    | Fail
    | NoResult


resultToString : Result -> Language -> String
resultToString r lang =
    case r of
        Success ->
            case lang of
                En ->
                    "Success"

                Fr ->
                    "Correct"

        Fail ->
            case lang of
                En ->
                    "Fail"

                Fr ->
                    "Faux"

        NoResult ->
            "\u{00A0}"


description : Language -> String
description lang =
    case lang of
        En ->
            "Once started you should guess if the displayed square "
                ++ "is a Light or a Dark one. This excercice may improve "
                ++ "your chess vision."

        Fr ->
            "Une fois commencé vous devez deviner si la case proposée est "
                ++ "foncée ou claire. Cette excercice devrait améliorer votre "
                ++ "vision au échecs."


start : Language -> String
start lang =
    case lang of
        En ->
            "Start"

        Fr ->
            "Commencer"


dark : Language -> String
dark lang =
    case lang of
        En ->
            "Dark"

        Fr ->
            "Foncée"


light : Language -> String
light lang =
    case lang of
        En ->
            "Light"

        Fr ->
            "Claire"


score : Language -> String
score lang =
    case lang of
        En ->
            "Your score is: "

        Fr ->
            "Votre score est\u{00A0}:"


restart : Language -> String
restart lang =
    case lang of
        En ->
            "Restart"

        Fr ->
            "Recommencer"


isaDarkSquare : Language -> String
isaDarkSquare lang =
    case lang of
        En ->
            " is a Dark square."

        Fr ->
            " est une case Foncée."


isaLightSquare : Language -> String
isaLightSquare lang =
    case lang of
        En ->
            " is a Light square."

        Fr ->
            " est une case Claire."
