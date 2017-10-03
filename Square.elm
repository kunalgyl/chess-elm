module Square exposing(..)
import Matrix exposing(..)
import Char as Ch

type alias Square = Location
--type alias for matrix location (of Board)

sq : Int -> Int -> Square
--checks if int pair forms valid square and converts it
sq r c = 
    let 
        s = (r,c)
        valid = isValid s
    in 
        if valid 
        then s 
        else Debug.crash ("Invalid square: (" ++ (toString r)++ ", "++ (toString c)++ ")")

isValid : Square -> Bool
--checks if location is valid
isValid sq = 
    let 
        (r,c) = sq
    in 
        (r >= 0) && (r <= 7) && (c >= 0) && (c <=7)

toStr : Square -> String
--converts location to string represetnation of square (chess notation)
toStr s =
    let 
        (r,c) = s
    in 
        (toAlpha (c+1)) ++ (toString (r+1))

toAlpha : Int -> String
--converts int to capital alphabet (1 to A, 2 to B)
toAlpha i =
    if (i >= 1) && (i <= 8)
    then (i + 64) |> Ch.fromCode |> String.fromChar
    else Debug.crash "invalid column: '" ++ (toString i) ++ "'"

toColorString : Square -> String
--converts location to its color as a square on the chessboard
toColorString s = 
    let (r,c) = s in
    let idx = r + c in
    if (idx%2 == 0) then "#00e68a" else "white"

toOtherColorString : Square -> String
--converts location to its color as a square on the chessboard
toOtherColorString s =
    let (r,c) = s in
    let idx = r + c in
    if (idx%2 == 0) then "#e6e600" else "#ffff66"

