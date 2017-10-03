module Piece exposing (..)

import PColor exposing(..)

type Piece = P | Kn | B | R | Q | K

toPoint : Piece -> Float
--converts piece to point value
toPoint p = 
    case p of --Values due to (Staunton 1870, 30–31)
        K  -> 1000.00
        Q  -> 9.94
        R  -> 5.48
        B  -> 3.50
        Kn -> 3.05
        P  -> 1.00

toPointSimple : Piece -> Int
--converts piece to simple point value (for display purposes)
toPointSimple p = 
    case p of
        K  -> 0
        Q  -> 9
        R  -> 5
        B  -> 3
        Kn -> 3
        P  -> 1

toPiece : Char -> Piece
--converts character to piece (useful for converting string representation of board)
toPiece c =
    case c of
        'K' -> K
        'Q' -> Q
        'r' -> R
        'b' -> B
        'k' -> Kn
        'p' -> P
        _   -> Debug.crash ("incorrect piece: '" ++ (String.fromChar c) ++ "'")

toStringAlt : Piece -> String
--converts piece to a unicode string representing the piece of the approrpriate color
--for debugging
toStringAlt pc =
    case pc of
        K  -> "K"
        Q  -> "Q"
        R  -> "r"
        B  -> "b"
        Kn -> "k"
        P  -> "p"

toString : Piece -> Color -> String
--converts piece to a unicode string representing the piece of the approrpriate color
toString pc p =
    case pc of
        K  -> if (p == White) then "♔" else "♚"
        Q  -> if (p == White) then "♕" else "♛"
        R  -> if (p == White) then "♖" else "♜"
        B  -> if (p == White) then "♗" else "♝"
        Kn -> if (p == White) then "♘" else "♞"
        P  -> if (p == White) then "♙" else "♟"
