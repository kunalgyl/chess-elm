module GameApp exposing(..)

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
import GameEval exposing(..)
import Keyboard exposing (..)
import Task exposing (..)
import Char exposing(..)

initialModel : (Model, Cmd Msg)
--the model in MVC
initialModel = ({ board = startingBoard
               , clicked = Nothing
               , moves = []
               , castles = {wsc = True, wlc = True, bsc = True, blc = True}
               , kposs = {w = (0,4), b = (7,4)}
               , turn = White
               , black_captured = []
               , white_captured = []
               , mv_history = []
               , check_sq = Nothing
               , history = []
               , paused = False
               , black_time = 3000
               , white_time = 3000
               , status = Progress
               , boards  = [startingBoard]
               , viewing_history = False
               , mv_history_idx = Nothing
               , black_ai = False
               , white_ai = False
               , suggestion = Nothing
               , evaluate = False
               , evaluation = 0.0
               , second_rank_move_file = Nothing
               , fen_content = ""
               , previous_delay = Nothing
               , depth = 2
               }, Cmd.none)

makeNewModel : Model -> Square -> Square -> Model
-- given a model and a pair of squares, makes a move (moving piece from first square to second)
makeNewModel m sq_s s =
  let (new_board, new_castles, captured, new_kposs, new_srm, mv) = makeMove m.board m.castles sq_s s m.kposs in
    { m
    | board = new_board
    , clicked = Nothing
    , moves = []
    , castles = new_castles
    , kposs = new_kposs
    , turn = PColor.otherColor m.turn
    , black_captured = m.black_captured ++ (if ((m.turn==Black)&&(isJust captured)) 
                                            then [justGet captured "clickSquare - impossible"] else [])
    , white_captured = m.white_captured ++ (if ((m.turn==White)&&(isJust captured)) 
                                            then [justGet captured "clickSquare - impossible"] else [])
    , mv_history = mv::m.mv_history
    , check_sq = 
      let next_player = PColor.otherColor m.turn in
      kingInCheck new_board next_player
    , history = (PrevModel {m | history = [], suggestion = Nothing, mv_history = [], boards = []})::m.history -- |> List.take 10
    , paused = m.paused
    , black_time = m.black_time
    , white_time = m.white_time 
    , status = 
      let other_player = (PColor.otherColor m.turn) in
      if (List.isEmpty (validSquares new_board new_castles other_player new_srm new_kposs)) 
      then (if (isJust (kingInCheck new_board other_player)) then (Victory m.turn) 
           else Draw) 
      else Progress 
    , boards = new_board::m.boards
    , suggestion = Nothing 
    , evaluation = evaluate new_board new_castles new_srm
    , second_rank_move_file = new_srm
    , previous_delay = Nothing
    }

makeNewModelSimple : Model -> Square -> Square -> Model
-- simple version of makeNewModel, for evaluation purposes
makeNewModelSimple m sq_s sq_d = 
  let (new_board, new_castles, _, new_kposs, new_srm, _) = makeMove m.board m.castles sq_s sq_d m.kposs in
   { m
   | board = new_board
   , castles = new_castles
   , kposs = new_kposs
   , turn = PColor.otherColor m.turn
   , second_rank_move_file = new_srm
   }

clickSquare : Model -> Square -> Model
--updates the model on a square being clicked (either a move made, or a piece selected/deselected)
clickSquare m s =   
  if List.member s m.moves then
    -- make move
    let sq_s = justGet m.clicked "clickSquare - impossible" in -- m.moves not empty, something clicked
    (makeNewModel m sq_s s)
  else
    -- other selection
    let bsq = justGet (Matrix.get s m.board) "clickSquare - impossible" in
    let (c, ms) = case (bsq,(m.paused||m.viewing_history)) of
                  (_,True)          -> (Nothing, [])
                  (Empty,_)         -> (Nothing, [])
                  (NotEmpty p pc,_) -> if ((isJust m.clicked) && ((justGet m.clicked "clickSquare - impossible")==s)) 
                                   then (Nothing, []) else
                                   -- clicking on highlighted piece unhighlights it, I preferred this
                                   if (p == m.turn) && (not (checkAI m p))
                                     then (Just s, sqMoves m.board m.castles s m.second_rank_move_file m.kposs)
                                     else (Nothing, []) in
    { m | clicked = c, moves = ms}

checkAI : Model -> Color -> Bool
checkAI m p = if (p == White) then m.white_ai else m.black_ai

changeIdx : Model -> Int -> Model
--updates the model if an index (of move history) is selected for preview
changeIdx model idx = 
  let new_idx = if (idx < 0) then 0 else (if (idx > (List.length model.mv_history)) 
    then (List.length model.mv_history) else idx) in
  if (new_idx == 0) then {model | board = (justGet (List.head model.boards) "update - impossible")
                     , viewing_history = False, mv_history_idx = Nothing}
  else {model | board = (justGet (takenth model.boards new_idx) "update - impossible")
       , viewing_history = True, moves = [], clicked = Nothing
       , check_sq = Nothing,  mv_history_idx = Just new_idx, suggestion = Nothing }

maybeDeficitColor : Model -> Color -> Maybe Int
--computer a maybe of the material deficit of a color
maybeDeficitColor m p =
  let bv = m.black_captured |> List.map Piece.toPointSimple |> List.foldr (+) 0 in
  let wv = m.white_captured |> List.map Piece.toPointSimple |> List.foldr (+) 0 in
  if (p == White) 
    then (if (wv > bv) then Just (wv-bv) else Nothing)
    else (if (bv > wv) then Just (bv-wv) else Nothing)

subscriptions : Model -> Sub Msg
--time (and maybe keyboard) subscription
subscriptions model =
  Sub.batch [ Time.every (100 * Time.millisecond) (\_ -> Tick)
            , Time.every (200 * Time.millisecond) (\_ -> CheckAI)
            , Keyboard.downs (\keyCode -> if keyCode == 37 then (MvIndexUpdate 1) else Noop)
            , Keyboard.downs (\keyCode -> if keyCode == 39 then (MvIndexUpdate -1) else Noop) ]

aiMove : Model -> Task x (Square, Square)
aiMove m =
  let mv = findMoveNegaMax m in
  Task.succeed (justGet mv "aiMove - impossible")

update : Msg -> Model -> (Model, Cmd Msg)
--updates model given msg
update msg model =
    case msg of
        ClickSquare s   -> 
          let new_m = clickSquare model s in
          let new_cmd = Cmd.none in
            -- if ((new_m.black_ai) && (new_m.turn == Black) && (statusIsProgress new_m.status))
            -- then Task.perform MoveAI (aiMove new_m) else Cmd.none in 
          (new_m, new_cmd)
        Reset           -> (Tuple.first initialModel, Cmd.none)
        Previous        -> 
          if (List.isEmpty model.history) then (model, Cmd.none) else
          let (PrevModel prev_m) = (justGet (List.head model.history) "update - impossible") in 
          let new_m = {prev_m | black_time = model.black_time, white_time = model.white_time
                 , moves = [], clicked = Nothing, mv_history = (justGet (List.tail model.mv_history) "update - impossible")
                 , history = (justGet (List.tail model.history) "update - impossible")
                 , boards = (justGet (List.tail model.boards) "update - impossible")
                 , previous_delay = Just 0} in
          let new_cmd = Cmd.none in
            -- if ((new_m.black_ai) && (new_m.turn == Black) && (statusIsProgress new_m.status))
            -- then Cmd.none else Cmd.none in 
            --then Task.perform MoveAI (aiMove new_m) else Cmd.none in 
          (new_m, new_cmd)
        Tick            -> (if model.paused||(model.status /= Progress) then model else
                          let new_m = 
                           if (model.turn==White) then {model | white_time=(model.white_time-1)} 
                           else {model | black_time = (model.black_time-1)}
                          in if (new_m.black_time == 0) then { new_m | status = Victory White}
                          else if (new_m.white_time == 0) then { new_m | status = Victory Black}
                          else new_m, Cmd.none)
        Pause           -> ({model | paused = (not model.paused), moves = [], clicked = Nothing} ,Cmd.none)
        Increment p     -> (if (p == White) then {model | white_time = model.white_time + 200} 
                         else {model | black_time = model.black_time + 200}, Cmd.none )
        Decrement p     -> (if (p == White) then 
          {model | white_time = if (model.white_time > 200) then (model.white_time - 200) else model.white_time} 
                         else 
          {model | black_time = if (model.black_time > 200) then (model.black_time - 200) else model.black_time} , Cmd.none )
        MvHistory idx   -> ((changeIdx model idx), Cmd.none)
        MvIndexUpdate i -> (if (not (isJust model.mv_history_idx)) then 
                            (changeIdx model i) else 
                              (changeIdx model ((justGet model.mv_history_idx "update - impossible") + i)), Cmd.none)
        Noop            -> (model, Cmd.none)
        TogAI p         -> 
          let new_m = if (p == White) then {model|white_ai = not model.white_ai} 
                      else {model|black_ai = not model.black_ai} in
          let new_cmd = Cmd.none in
            -- if ((new_m.black_ai) && (new_m.turn == Black) && (statusIsProgress new_m.status))
            -- then Task.perform MoveAI (aiMove model) else Cmd.none in 
          (new_m, new_cmd)
        Suggest         -> if isJust model.suggestion then ({model | suggestion = Nothing}, Cmd.none) else
                           (model, Task.perform MakeSugg (findMoveAsync model))
        MakeSugg mmv    -> ({model | suggestion = mmv}, Cmd.none) 
        TogEval         -> ({model | evaluate = not (model.evaluate) }, Cmd.none)
        MoveAI mv       -> 
          let (sq_s, sq_d) = mv in
          ((makeNewModel model sq_s sq_d ), Cmd.none)
        Change s        -> ({model | fen_content = s}, Cmd.none)
        UpdateFEN       -> 
          let new_m = (fromFEN model.fen_content) in 
          let new_kposs = {w = (findKing new_m.board White), b = (findKing new_m.board Black)} in
          let new_srm = new_m.second_rank_move_file in
          ({new_m | fen_content = model.fen_content
                  , kposs = new_kposs
                  , status = 
                        let other_player = PColor.otherColor new_m.turn in
                        if (List.isEmpty (validSquares new_m.board new_m.castles new_m.turn new_srm new_kposs)) 
                        then (if (isJust (kingInCheck new_m.board new_m.turn)) then (Victory other_player) 
                             else Draw) 
                        else Progress }
                  , Cmd.none)
        CheckAI         -> if ((model.black_ai) && (model.turn == Black) && (statusIsProgress model.status)) then
                           case model.previous_delay of
                              Nothing -> (model, Task.perform MoveAI (aiMove model))
                              Just 5 -> ({model|previous_delay = Nothing}, Task.perform MoveAI (aiMove model))
                              Just delay -> ({model|previous_delay = Just (delay + 1)}, Cmd.none)
                           else (model, Cmd.none) 
        TogDepth        -> ({model | depth = if (model.depth == 2) then 3 else 2}, Cmd.none)

statusIsProgress : Status -> Bool
-- checks is the status is Progress
statusIsProgress s = 
  case s of
    Progress -> True
    _        -> False

view : Model -> Html Msg
--creates Html view of the given model
view m = 
  let standard_stuff = 
    [ (drawCapturedCastles Black m.black_captured m.castles (maybeDeficitColor m Black) m.black_time)
    , (drawBoard m.board m.clicked m.check_sq m.moves m.suggestion) 
    , (drawCapturedCastles White m.white_captured m.castles (maybeDeficitColor m White) m.white_time)] in
  let game_window = 
    case m.status of
      Draw      -> div [style [( "display" , "inline-block")]] (standard_stuff ++ [drawBox "It's A Draw..."])
      Victory p -> div [style [( "display" , "inline-block")]] (standard_stuff ++ [drawBox ((PColor.toString p) ++ " Wins!")])
      Progress  -> div [style [( "display" , "inline-block")]] standard_stuff in
  div [style [( "margin", "0 auto" ), ( "width", "610px") , ("margin-top", "50px")]] 
  [game_window, (drawHistory m.mv_history m.depth m.evaluate m.black_ai m.evaluation m.paused)]

main =
--where the magic happens
    Html.program { init = initialModel
                 , view = view
                 , update = update 
                 , subscriptions = subscriptions}


--Engine stuff, in GameApp (hopefully only temporarily)

modelMoves : Model -> (List (Square, Square))
-- given model, returns number of moves next player has
modelMoves m =
  validMoves m.board m.castles m.turn m.second_rank_move_file m.kposs

findMaxBy : List a -> (a -> comparable) -> Maybe a
-- given a list, finds the maximum element according to a given function
findMaxBy l f =
  let fold_helper a acc =
    let comp = (f a) in
    if (comp > (Tuple.first acc)) then (comp, a) else acc in
  case l of
    []   -> Nothing
    a::l -> List.foldr fold_helper ((f a), a) l |> Tuple.second |> Just

findMinBy : List a -> (a -> comparable) -> Maybe a
-- given a list finds the minimum element accordin to a given function
findMinBy l f =
  let fold_helper a acc =
    let comp = (f a) in
    if (comp < (Tuple.first acc)) then (comp, a) else acc in
  case l of
    []   -> Nothing
    a::l -> List.foldr fold_helper ((f a), a) l |> Tuple.second |> Just

findMoveAsync : Model -> Task x (Maybe (Square, Square))
-- tries to implement the findMove asynchronously, but it still causes the game to 
-- hault until this returns
findMoveAsync m =
  Task.succeed (findMoveNegaMax m)

findMoveNegaMax : Model -> Maybe (Square, Square)
-- finds the optimum negamax move, if any ((0,0), (0,0)) is a dummy move to make
-- everythign work out, should only be returned when the game ends
findMoveNegaMax m = 
  let poss_mv = (negamax m.depth -10000 10000 m.turn m) |> Tuple.second in
  if poss_mv == ((0,0),(0,0)) then Nothing else Just poss_mv

negamax : Int -> Float -> Float -> Color -> Model -> (Float, (Square, Square))
-- performs negamax with ab pruning
negamax d a b p m =
  case m.status of
    Victory pl_w -> (if (p == pl_w) then 10000.0 else -10000.0, ((0,0),(0,0)))
    Draw         -> (0,((0,0),(0,0)))
    _            ->
      if (d==0) then ((evaluate m.board m.castles m.second_rank_move_file) * 
        (if (p == White) then -1.0 else 1.0), ((0,0),(0,0))) else
      let mvs = (modelMoves m) |> List.sortBy (moveAttackComp m.board) in
      let evalChild a l_mvs (best_value, best_move) =
        case l_mvs of
          []                  -> (best_value, best_move)
          (sq_s, sq_d)::l_mvs ->
            let new_m = (makeNewModelSimple m sq_s sq_d) in
            let v = -1 * Tuple.first (negamax (d-1) -b -a (PColor.otherColor p) new_m) in
            let (new_best, new_best_move) = if (v > best_value) 
                then (v, (sq_s, sq_d)) else (best_value, best_move) in
            let new_a = Basics.max a v in
            if (new_a >= b) then (new_best, new_best_move) 
                else evalChild new_a l_mvs (new_best, new_best_move) in
      evalChild a mvs (-10000, ((0,0),(0,0)))

moveAttackComp : Board -> (Square, Square) -> Int
-- given a board and a move, returns 1 if the move is an attack
-- used to sort moves by attacks in ab pruning
moveAttackComp b (sq_s, sq_d) =
  if (isOccupied b sq_d) then 1 else 0

-- FEN stuff

fromFEN : String -> Model
fromFEN s =
  let ss = String.split " " s in
  case ss of
    bs::ps::cs::es::_ -> 
      let new_board = processBoard bs in
      let new_turn = if (ps=="b") then Black else White in
      let new_wsc = String.contains "K" cs in
      let new_wlc = String.contains "Q" cs in
      let new_bsc = String.contains "k" cs in
      let new_blc = String.contains "q" cs in
      let new_castles = {wsc = new_wsc, wlc = new_wlc, bsc = new_bsc, blc = new_blc} in 
      let c_es = es |> String.toList |> List.head |> Maybe.withDefault '_' in
      let c_es_d = c_es |> fileToInt |> (+) 48 |> Char.fromCode in
      let new_srm = if (Char.isDigit c_es_d) then Just (Basics.min 7 (ctoi c_es_d)) else Nothing in
      let im = Tuple.first initialModel in
      {im | board = new_board, turn = new_turn, castles = new_castles 
          , second_rank_move_file = new_srm}
    _ -> Tuple.first initialModel

fileToInt : Char -> Int
fileToInt c = Basics.max 0 (Basics.min 7 (Char.toCode c - 97))

{-

findMove : Model -> Maybe (Square, Square)
-- given a model, finds the best move using minimax with ab pruning, now depracated
-- see findnegamax
findMove m = 
  let mvs = (modelMoves m) |> List.sortBy (moveAttackComp m.board) in
  let fn_min (sq_s, sq_d) (best_s, best_m) = 
    let new_m = (makeNewModel m sq_s sq_d) in
    let new_beta = evMin 1 best_s 1000 new_m in
    if new_beta > best_s then (new_beta, (sq_s, sq_d)) else (best_s, best_m) in
  let fn_max (sq_s, sq_d) (best_s, best_m) = 
    let new_m = (makeNewModel m sq_s sq_d) in
    let new_alpha = evMax 1 -1000 best_s new_m in
    if new_alpha < best_s then (new_alpha, (sq_s, sq_d)) else (best_s, best_m) in
  let res = 
    if (m.turn == White) then mvs |> List.foldr fn_max (1000, ((0,0),(0,0)))
    else mvs |> List.foldr fn_min (-1000, ((0,0),(0,0)))
  in
    if (Tuple.second res == ((0,0),(0,0))) then Nothing
    else Just (Tuple.second res)

evMax : Int -> Float -> Float -> Model -> Float
-- ab pruning function
evMax d alpha beta m =
  --let tmp = if (isJust (m.check_sq)) then Debug.crash ((PColor.toString m.turn) ++ " " ++ (boardToString m.board)) else 0 in
  --if (isJust (m.check_sq)) then 0.0 else
  case m.status of
    Victory p -> if (p == White) then 1000.0 else -1000.0
    Draw      -> 0
    _         ->
        if (d==0) then (evaluate m.board m.castles) else 
        let mvs = (modelMoves m) |> List.sortBy (moveAttackComp m.board) in
        let evalMax alpha beta mvs = 
          let is_empty_mvs = List.isEmpty mvs in
          let new_alpha = if is_empty_mvs then alpha else 
            let (sq_s, sq_d) = justGet (List.head mvs) "evMax - impossible" in
            let new_m = (makeNewModelSimple m sq_s sq_d) in
            Basics.max alpha (evMin (d-1) alpha beta new_m) in
          if is_empty_mvs then alpha else if (beta <= new_alpha) then beta 
          else evalMax new_alpha beta (justGet (List.tail mvs) "evMax - impossible") in
        evalMax alpha beta mvs

evMin : Int -> Float -> Float -> Model -> Float
-- ab pruning function
evMin d alpha beta m =
  case m.status of
    Victory p -> if (p == White) then 1000.0 else -1000.0
    Draw      -> 0
    _         ->
        if (d==0) then (evaluate m.board m.castles) else 
        let mvs = (modelMoves m) |> List.sortBy (moveAttackComp m.board) in
        let evalMin alpha beta mvs = 
          let is_empty_mvs = List.isEmpty mvs in
          let new_beta = if is_empty_mvs then beta else 
            let (sq_s, sq_d) = justGet (List.head mvs) "evMax - impossible" in
            let new_m = (makeNewModelSimple m sq_s sq_d) in
            Basics.min beta (evMax (d-1) alpha beta new_m) in
          if is_empty_mvs then beta else if (new_beta <= alpha) then alpha 
          else evalMin alpha new_beta (justGet (List.tail mvs) "evMax - impossible") in
        evalMin alpha beta mvs
-}


