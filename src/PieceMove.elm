module PieceMove exposing (..)

import Common as C


isOutOfRange : C.Square -> Bool
isOutOfRange ( c, r ) =
    c > 0 && c < 9 && r > 0 && r < 9


rookMoves : C.Square -> List C.Square
rookMoves ( col, row ) =
    let
        colMoves =
            List.range 1 8
                |> List.filter ((/=) row)
                |> List.map (Tuple.pair col)

        rowMoves =
            List.range 1 8
                |> List.filter ((/=) col)
                |> List.map (C.flip Tuple.pair row)
    in
    List.append rowMoves colMoves


bishopMoves : C.Square -> List C.Square
bishopMoves ( col, row ) =
    let
        risingDiagonal =
            List.range 1 8
                |> List.map (\x -> Tuple.pair x <| row - col + x)

        descendingDiagonal =
            List.range 1 8
                |> List.map (\x -> Tuple.pair x <| row + col - x)
    in
    List.append risingDiagonal descendingDiagonal
        |> List.filter isOutOfRange
        |> List.filter ((/=) ( col, row ))


queenMoves : C.Square -> List C.Square
queenMoves square =
    List.append (rookMoves square) (bishopMoves square)


knightMoves : C.Square -> List C.Square
knightMoves ( col, row ) =
    [ ( col + 1, row + 2 )
    , ( col + 2, row + 1 )
    , ( col + 2, row - 1 )
    , ( col + 1, row - 2 )
    , ( col - 1, row - 2 )
    , ( col - 2, row - 1 )
    , ( col - 2, row + 1 )
    , ( col - 1, row + 2 )
    ]
        |> List.filter isOutOfRange
