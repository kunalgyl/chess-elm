module PColor exposing (..)

type Color = Black | White
--representation for color (piece color and player color)

otherColor : Color -> Color
--converts color to other color
otherColor c =
    case c of 
        Black -> White
        _ -> Black

toColor : Char -> Color
--converts character to color (for converting string representation of board)
toColor c =
    case c of
        'B' -> Black
        'W' -> White
        _   -> Debug.crash ("incorrect color: '" ++ (String.fromChar c) ++ "'")

toString : Color -> String
--converts color to string
toString c =
    case c of
        Black -> "Black"
        _     -> "White"

toStringAlt : Color -> String
--converts color to string
toStringAlt c =
    case c of
        Black -> "B"
        _     -> "W"

pawnRank : Color -> Int
--returns the rank at which pawns of the given player start
pawnRank c =
    case c of
        White -> 1
        _     -> 6
