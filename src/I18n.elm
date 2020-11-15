module I18n exposing (..)

import Common


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


result : Language -> Result -> String
result lang r =
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


piece : Language -> Common.Piece -> String
piece lang p =
    case p of
        Common.NoPiece ->
            case lang of
                En ->
                    "\u{00A0}"

                Fr ->
                    "\u{00A0}"

        Common.Queen ->
            case lang of
                En ->
                    "Q"

                Fr ->
                    "D"

        Common.Knight ->
            case lang of
                En ->
                    "K"

                Fr ->
                    "C"

        Common.Rook ->
            case lang of
                En ->
                    "R"

                Fr ->
                    "T"

        Common.Bishop ->
            case lang of
                En ->
                    "B"

                Fr ->
                    "F"


colorVision : Language -> String
colorVision lang =
    case lang of
        En ->
            "Color Vision"

        Fr ->
            "Vision des Couleurs"


pieceMoves : Language -> String
pieceMoves lang =
    case lang of
        En ->
            "Piece Moves"

        Fr ->
            "Mouvements des Pièces"


positionToString : Language -> Common.Position -> String
positionToString lang position =
    Tuple.second position
        |> Common.squareToString
        |> (++) (piece lang <| Tuple.first position)


description : Language -> String
description lang =
    case lang of
        En ->
            "Choose one of the available games below:"

        Fr ->
            "Choisissez un des jeux disponiblent ci-dessous\u{00A0}:"


goBackHome : Language -> String
goBackHome lang =
    case lang of
        En ->
            "Go back home"

        Fr ->
            "Retour à la page d'accueil"


colorVisionDescription : Language -> String
colorVisionDescription lang =
    case lang of
        En ->
            "Once started you should guess if the displayed square "
                ++ "is a Light or a Dark one. This excercice may improve "
                ++ "your chess vision."

        Fr ->
            "Une fois la partie commencée vous devez deviner si la case proposée est "
                ++ "foncée ou claire. Cette excercice devrait améliorer votre "
                ++ "vision aux échecs."


pieceMoveDescription : Language -> String
pieceMoveDescription lang =
    case lang of
        En ->
            "Once started you should input all square where the displayed piece can "
                ++ "move."

        Fr ->
            "Une fois la partie commencée vous devez entrer l'ensemble des cases "
                ++ "sur lesquelles la pièce proposée peut se déplacer."


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


useTimer : Language -> String
useTimer lang =
    case lang of
        En ->
            "Use timer?"

        Fr ->
            "Utiliser le chronomètre\u{00A0}?"
