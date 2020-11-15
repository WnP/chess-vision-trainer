module Common exposing (..)

import Tuple


flip : (a -> b -> c) -> b -> a -> c
flip func b a =
    func a b


removeIndexFromList : List a -> Int -> List a
removeIndexFromList l i =
    List.drop (i + 1) l
        |> List.append (List.take i l)


rowChars : String
rowChars =
    "12345678"


colChars : String
colChars =
    "abcdefgh"


colAndRowChars : String
colAndRowChars =
    colChars ++ rowChars


type alias Square =
    ( Int, Int )


emptySquare : Square
emptySquare =
    ( 0, 0 )


squareToString : Square -> String
squareToString ( x, y ) =
    if x == 0 then
        ""

    else
        String.fromChar (Char.fromCode <| x + 96) ++ String.fromInt y


stringToSquare : String -> Square
stringToSquare s =
    ( String.left 1 s
        |> flip String.indexes colChars
        |> List.head
        |> Maybe.withDefault 0
        |> (+) 1
    , String.right 1 s
        |> flip String.indexes rowChars
        |> List.head
        |> Maybe.withDefault 0
        |> (+) 1
    )


allSquares : List Square
allSquares =
    List.range 1 8
        |> List.concatMap (Tuple.pair >> flip List.map (List.range 1 8))


type Piece
    = NoPiece
    | Queen
    | Knight
    | Rook
    | Bishop


allPieces : List Piece
allPieces =
    [ Queen, Knight, Rook, Bishop ]


type alias Position =
    ( Piece, Square )


emptyPosition : Position
emptyPosition =
    ( NoPiece, emptySquare )


isOutOfRange : Square -> Bool
isOutOfRange ( c, r ) =
    c > 0 && c < 9 && r > 0 && r < 9


rookMoves : Square -> List Square
rookMoves ( col, row ) =
    let
        colMoves =
            List.range 1 8
                |> List.filter ((/=) row)
                |> List.map (Tuple.pair col)

        rowMoves =
            List.range 1 8
                |> List.filter ((/=) col)
                |> List.map (flip Tuple.pair row)
    in
    List.append rowMoves colMoves


bishopMoves : Square -> List Square
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


queenMoves : Square -> List Square
queenMoves square =
    List.append (rookMoves square) (bishopMoves square)


knightMoves : Square -> List Square
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
