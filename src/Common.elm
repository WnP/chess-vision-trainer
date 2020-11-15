module Common exposing (..)


flip : (a -> b -> c) -> b -> a -> c
flip func b a =
    func a b


type alias Square =
    ( Int, Int )


emptySquare : Square
emptySquare =
    ( 0, 0 )
