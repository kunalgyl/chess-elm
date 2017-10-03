module Board exposing(..)

import Piece exposing(..)
import PColor exposing(..)
import Matrix exposing(..)
import Char exposing (..)
import Array exposing (..)

type BSquare = Empty | NotEmpty Color Piece
--type for squares on the board

type alias Board = Matrix BSquare
--board is a matrix of squares

startingBoardStringList : List String
--string representation of startingBoard
startingBoardStringList = 
    ["Wr Wk Wb WQ WK Wb Wk Wr"
    ,"Wp Wp Wp Wp Wp Wp Wp Wp"
    ,"E  E  E  E  E  E  E  E" 
    ,"E  E  E  E  E  E  E  E"
    ,"E  E  E  E  E  E  E  E"
    ,"E  E  E  E  E  E  E  E"
    ,"Bp Bp Bp Bp Bp Bp Bp Bp"
    ,"Br Bk Bb BQ BK Bb Bk Br"]

startingBoard : Board
--initial board
startingBoard = startingBoardStringList |> List.map (\s -> (List.map toBSquare (String.words s))) 
                                        |> Matrix.fromList
   
    
toBSquare : String -> BSquare
--converts string into BSquares (used to convert string representation of startingBoard)
toBSquare s =
    case s of
        "E" -> Empty
        _   -> case (String.toList s)
                of [c, p] -> (NotEmpty (toColor c) (toPiece p))
                   _      -> Debug.crash ("incorrect BSquare string: '" ++ s ++ "'")

toString : BSquare -> String
--converts BSquare to string
toString bsq =
    case bsq of 
        NotEmpty p pc -> Piece.toString pc p
        Empty         -> ""

boardToString : Board -> String
boardToString b =
    let bsqToStr bsq =
        case bsq of 
            Empty -> "E  "
            NotEmpty p pc -> (PColor.toStringAlt p) ++ (Piece.toStringAlt pc) ++ " " in
    b |> Matrix.toList |> List.map (\l -> List.map bsqToStr l) |> List.map String.concat |> String.concat

-- FEN stuff

fenToPiece : Char -> BSquare
-- converts fen character to piece
fenToPiece c =
    if (c=='_') then Empty else
    let p = if (Char.isUpper c) then White else Black in
    let (pc, b) = 
        case (Char.toLower c) of
            'k' -> (K, True)
            'q' -> (Q, True)
            'r' -> (R, True)
            'b' -> (B, True)
            'n' -> (Kn, True)
            'p' -> (P, True)
            _   -> (P, False) in --dummy, to see incorrect fen strings
    if b then (NotEmpty p pc) else Empty

ctoi : Char -> Int
-- converts char to int 
ctoi c =
    c |> String.fromChar |> String.toInt |> Result.toMaybe |> Maybe.withDefault 0

ctos : Char -> String
-- converts char to a string (leaves all but integers alone, replaces ints with apt spaces)
ctos c =
    if (not (isDigit c)) then (String.fromChar c) else
    let i = ctoi c in
    let j = Basics.max 0 (Basics.min 8 i) in
    String.repeat j "_"

processRow : String -> (Array BSquare)
processRow s =
    s |> String.toList |> List.map ctos |> String.concat |> String.toList |> 
        List.take 8 |> List.map fenToPiece |> Array.fromList

processBoard : String -> Board
processBoard s =
    s |> String.split "/" |> List.map processRow |> List.reverse |> Array.fromList