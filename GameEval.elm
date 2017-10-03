module GameEval exposing(..)

import Piece exposing(..)
import PColor exposing(..)
import Matrix exposing(..)
import Square exposing(..)
import Board exposing(..)
import Game exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import GameDraw exposing(..)
import Time exposing (..)

type alias Model =
  { 
    board : Board,
    clicked : Maybe Square,
    moves : List Square,
    castles : CastleBools,
    kposs : KingPoss,
    turn : Color,
    black_captured : List Piece,
    white_captured : List Piece,
    mv_history : List MoveHist,
    check_sq : Maybe Square,
    history : List PrevModel,
    paused : Bool,
    black_time : Int, 
    white_time : Int,
    status : Status,
    boards : List Board,
    viewing_history : Bool, 
    mv_history_idx : Maybe Int,
    black_ai : Bool,
    white_ai : Bool,
    suggestion : Maybe (Square, Square),
    evaluate : Bool,
    evaluation : Float,
    second_rank_move_file : Maybe Int,
    fen_content : String,
    previous_delay : Maybe Int,
    depth : Int
  }
      
type PrevModel = PrevModel (Model)

type Status = Progress | Draw | Victory Color

development : Board -> Color -> Int
-- finds the number of non-pawn pieces that have been developed (a bit primitive, but ok)
development b p =
  let l_loc_b = b |> Matrix.mapWithLocation (\loc a-> (loc,a)) |> Matrix.flatten in
  let p_rank = if (p == White) then 0 else 7 in
  let countDeveloped (loc, bsq) acc =
    case bsq of 
      (NotEmpty sq_p sq_pc) -> 
        if (sq_p /= p) then acc else
        case sq_pc of 
          Q  -> if (loc == (p_rank,3)) then acc else acc+1
          R  -> if ((loc == (p_rank,0))||(loc == (p_rank,7))) then acc else acc+1
          B  -> if ((loc == (p_rank,2))||(loc == (p_rank,5))) then acc else acc+1
          Kn -> if ((loc == (p_rank,1))||(loc == (p_rank,6))) then acc else acc+1   
          _  -> acc
      _                     -> acc
  in 
    l_loc_b |> List.foldr countDeveloped 0

material : Board -> Color -> Float
material b p =
  let countMaterial bsq acc =
    case bsq of
      (NotEmpty sq_p sq_pc) -> if (sq_p == p) then acc + (Piece.toPoint sq_pc) else acc
      _                     -> acc in
  b |> Matrix.flatten |> List.foldr countMaterial 0.0

centerControl : Board -> Color -> Int
centerControl b p =
  let center = [(3,3), (3,4), (4,3), (4,4)] in
  center |> List.map (sqInAttackTimes b p) |> List.foldr (+) 0

threats : Board -> Color -> (List Square) -> Int
threats b p sqs =
   sqs |> List.filter (isOccupiedByPl b (PColor.otherColor p)) |> List.length

pieceScope : (List Square) -> Int
pieceScope sqs = sqs |> List.length

evaluateColor : Board -> Color -> List Square -> Float
evaluateColor b p sqs = 
    0.1 * toFloat (development b p)
  + (material b p)
  -- + 0.2 * toFloat (centerControl b p)
  + 0.05 * toFloat (threats b p sqs)
  + 0.02 * toFloat (pieceScope  sqs)

evaluate : Board -> CastleBools -> Maybe Int -> Float
evaluate b cbs mi  = 
  let sqs_white = (validSquaresWithoutLegality b cbs White mi) in
  let sqs_black = (validSquaresWithoutLegality b cbs Black mi) in
  (evaluateColor b Black sqs_black) - (evaluateColor b White sqs_white)

