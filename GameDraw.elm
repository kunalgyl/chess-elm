module GameDraw exposing(..)

import Piece exposing(..)
import Square exposing(..)
import PColor exposing(..)
import Matrix exposing(..)
import Board exposing(..)
import Game exposing(..)
import Html exposing(..)
import Array exposing(..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg
--type for the messages
    = ClickSquare Square
    | Reset
    | Previous
    | Tick
    | Pause
    | Increment Color
    | Decrement Color
    | MvHistory Int
    | MvIndexUpdate Int
    | Noop
    | TogAI Color
    | Suggest
    | TogEval
    | MakeSugg (Maybe (Square, Square))
    | MoveAI (Square, Square)
    | Change String
    | UpdateFEN
    | CheckAI
    | TogDepth

-- the overall display was inspired by Lichess

isJustEq : Maybe a -> a -> Bool
--returns true if maybe is a just of same value as second argument
isJustEq ma a =
  case ma of
    Just an -> (an == a)
    _       -> False

secondsToString : Int -> String
-- converts number of seconds into a string
secondsToString sec =
  let minutes = sec // 60 in
  let seconds = sec % 60 in
  (Basics.toString minutes) ++ ":" ++ (if (seconds<10) then 
                                      ("0"++(Basics.toString seconds)) else
                                      (Basics.toString seconds))

buttonStyle : List (String, String) -> List (String, String)
buttonStyle sss =
  [ --("-moz-box-shadow", "inset 0px 1px 3px 0px #91b8b3")
  --, ("-webkit-box-shadow", "inset 0px 1px 3px 0px #91b8b3")
  --, ("box-shadow", "inset 0px 1px 3px 0px #91b8b3")
  ("background", "-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #768d87), color-stop(1, #6c7c7c))")
  , ("background", "-moz-linear-gradient(top, #768d87 5%, #6c7c7c 100%)")
  , ("background", "-webkit-linear-gradient(top, #768d87 5%, #6c7c7c 100%)")
  , ("background", "-o-linear-gradient(top, #768d87 5%, #6c7c7c 100%)")
  , ("background", "-ms-linear-gradient(top, #768d87 5%, #6c7c7c 100%)")
  , ("background", "linear-gradient(to bottom, #768d87 5%, #6c7c7c 100%)")
  , ("filter", "progid:DXImageTransform.Microsoft.gradient(startColorstr='#768d87', endColorstr='#6c7c7c',GradientType=0)")
  , ("background", "-color:#768d87")
  ,( "-moz-border-radius", "5px")
  , ("-webkit-border-radius", "5px")
  , ("border-radius", "5px")
  , ("border", "1px solid #566963")
  , ("display", "inline-block")
  , ("cursor", "pointer")
  , ("color", "#ffffff")
  , ("font-family", "Arial")
  , ("font-size", "10px")
  , ("font-weight", "bold")
  , ("margin-botton", "0.5px")
  --, ("padding", "7px 23px")
  , ("text-shadow", "0px -0.5px 0px #2b665e")
  , ("text-decoration", "none")
  , ("outline", "none")
  ] ++ sss

drawCapturedCastles : Color -> List Piece -> CastleBools -> Maybe Int -> Int -> Html Msg
-- draws the div that prints captured pieces information, time, as well as castles information
drawCapturedCastles p pcs c mi time =
  let sorted_pcs = pcs |> List.sortBy Piece.toPoint in
  let sc = if (p == White) then c.wsc else c.bsc in
  let lc = if (p == White) then c.wlc else c.blc in
  let toDiv pc = 
    let bsq = (NotEmpty (PColor.otherColor p) pc) in
    div [ attribute "class" "square" 
            , style [ ( "width", "15px" )
                   , ( "height", "15px" )
                   , ( "display" , "inline-block")
                   , ( "cursor", "default" )
                   , ( "text-align", "center" )
                   , ( "font-size", "12px")]]
            [div [] [text (Board.toString bsq)]] in
  let toCastleNot b s =
   [div [style [( "display" , "inline-block")
              , ( "cursor", "default" )
              , ( "float", "right" )
              , ( "font-size", "15px")
              , ( "height", "20px" )
              , ( "margin-left", "5px")
              , ( "color", if b then "green" else "red")]] 
              [text s]] in
  let maybeIntDiv = if (isJust mi) then 
              [div [style [( "display" , "inline-block")
              , ( "cursor", "default" )
              , ( "font-size", "12px")]]
              [text ("+"++ (Basics.toString (justGet mi "impossible - drawCapturedCastles")))]] else [] in
  let timeDiv = [div [style [( "display" , "inline-block")
              , ( "cursor", "default" )
              , ( "float", "right" )
              , ( "font-size", "15px")
              , ( "height", "20px" )
              , ( "margin-left", "5px")]] 
              [text (secondsToString (time//10))]] in
  let incrButton = [Html.button [onClick (Increment p), 
                                  style [( "margin-left", "5px" )
                                        , ( "float", "right" )]] [Html.text "+"]] in
  let decrButton = [Html.button [onClick (Decrement p), 
                                  style [( "margin-left", "5px" )
                                        , ( "float", "right" )]] [Html.text "-"]] in
  incrButton ++ timeDiv ++ decrButton ++ (toCastleNot sc "SC") ++ (toCastleNot lc "LC ") 
  ++ (List.map toDiv sorted_pcs) ++ maybeIntDiv 
                        |> div [style
                          [ ( "width", "400px" )
                          , ( "height", "20px" )
                          , ( "margin", "0 auto" )
                          , ( "border", "1px solid black" )
                          ]] 

checkSugg : Maybe (Square, Square) -> Square -> Bool
checkSugg msugg sq =
  if (isJust msugg) then 
    let sugg = (justGet msugg "checkSugg - impossible") in
    (sq == (Tuple.first sugg)) || (sq == (Tuple.second sugg))
  else False

drawBoard : Board -> Maybe Square -> Maybe Square -> (List Square) -> Maybe (Square, Square) -> Html Msg
-- draws the game board
drawBoard b ms ms_k mvs msugg =
    let sq_bg sq = 
      if (isJustEq ms_k sq) then "Crimson" else
      if (isJustEq ms sq) then "Aquamarine" else
      if List.member sq mvs then (if (isOccupied b sq) then "LightCoral" else (toOtherColorString sq)) else
      if (checkSugg msugg sq) then "purple" else
      (toColorString sq) in
    let toDiv sq bsq =
        div [ attribute "class" "square" 
            , style [ ("backgroundColor", (sq_bg sq))
                   , ( "width", "50px" )
                   , ( "height", "50px" )
                   , ( "display" , "inline-block")
                   , ( "text-align", "center" )
                   , ( "vertical-align", "top" )
                   , ( "cursor", if ((isOccupied b sq)||(List.member sq mvs)) then "pointer" else "default")
                   , ( "font-size", "40px")]
            , onClick (ClickSquare sq)]
            [div [] [text (Board.toString bsq)]]
    in Matrix.mapWithLocation toDiv b |> Array.toList |> List.reverse |> Array.fromList
                |> Matrix.flatten |> div [style
                          [ ( "width", "400px" )
                          , ( "height", "400px" )
                          , ( "margin", "0 auto" )
                          , ( "border", "1px solid black" )
                          , ( "z-index", "-1")
                          ]]

drawBox : String -> Html Msg
-- draws the box that pops when game has ended
drawBox s = 
  let text_div = div [style [( "font-size", "20px"), ("margin-left", "20px")]] [text s] in
  let reset_button = Html.button [onClick Reset, [( "margin", "30px" ),( "margin-left", "60px" )] |> style] [Html.text "⏏"] in
  div [style
        [ ( "width", "160px" )
        , ( "height", "100px" )
        , ( "margin", "0 auto" )
        , ( "border", "1px solid black" )
        , ( "z-index", "1")
        ]] [text_div, reset_button]

drawFenControls : Html Msg
drawFenControls = 
    let load_button = Html.button [onClick UpdateFEN, [( "margin-left", "7px" )] |> style] [Html.text "Load"] in
    let input_box = input [ placeholder "FEN", onInput Change, style [ ("width", "80px")
                                                                     , ( "margin-left", "5px")
                                                                     , ("height", "12px")
                                                                     , ("text-align", "center")
                                                                     ]] [] in
    div [style
          [ ( "width", "160px" )
          , ( "height", "20px" )
          , ( "margin", "0 auto" )
          , ( "border-bottom", "1px solid black" )
          ]] 
    [input_box, load_button]

drawMoveHistoryControls : Bool -> Html Msg
-- draws the move history controls div
drawMoveHistoryControls isprev = 
    let prev_mv_button = Html.button [onClick (MvIndexUpdate 1), [( "margin-left", "6px" )] |> style] [Html.text "⟸"] in
    let next_mv_button = Html.button [onClick (MvIndexUpdate -1), [( "margin-left", "6px" )] |> style] [Html.text "⟹"] in
    let first_mv_button = Html.button [onClick (MvIndexUpdate 1000), [( "margin-left", "6px" )] |> style] [Html.text "⟽"] in
    let last_mv_button = Html.button [onClick (MvHistory 0), [( "margin-left", "6px" )] |> style] [Html.text "⟾"] in
  div [style
        [ ( "width", "160px" )
        , ( "height", "20px" )
        , ( "margin", "0 auto" )
        , ( "border-bottom", "1px solid black" )
        ]] 
    [first_mv_button, prev_mv_button, next_mv_button, last_mv_button ]
                           
drawControls : Bool -> Html Msg
-- draws the game state controls (pause reset and undo move)
drawControls ispaused = 
  let reset_button = Html.button [onClick Reset, style [( "margin-left", "9px" )]] [Html.text "⏏"] in
  let back_button = Html.button [onClick Previous, style [( "margin-left", "9px" )]] [Html.text "↩"] in
  let pause_button = Html.button [onClick Pause, style [( "margin-left", "9px" )]] [Html.text (if ispaused then "▶" else "❚❚")] in
  let suggest_button = Html.button [onClick Suggest, style [( "margin-left", "9px" )]] [Html.text "?"] in
  div [style
        [ ( "width", "160px" )
        , ( "height", "20px" )
        , ( "margin", "0 auto" )
        , ( "border-bottom", "1px solid black" )
        ]] 
      [reset_button, back_button, pause_button, suggest_button ]

roundFloatString : Float -> String
-- converts a float to a string representing the rounded value of that float (two decimals)
roundFloatString f = 
  let characteristic = (Basics.truncate f) in
  let mantissa = Basics.abs (f - (toFloat characteristic)) in
  (Basics.toString characteristic) ++ "." ++ (Basics.toString (Basics.truncate (mantissa * 100.0)))

drawEval : Int -> Bool -> Bool -> Float -> Html Msg
drawEval depth evaluate ai evaluation = 
  let eval_button = Html.button [onClick TogEval, style [( "margin-left", "5px" )]] [Html.text "Eval"] in
  let tog_ai_button = Html.button [onClick (TogAI Black), style [ ( "margin-left", "5px" )
                                                                , ("color", if ai then "green" else "red")]] [Html.text "AI"] in
  let tog_depth_button = Html.button [onClick TogDepth, style [ ( "margin-left", "7px" )
                                                                ]] [Html.text (Basics.toString depth)] in
  let ev = div [style [( "display" , "inline-block")
              , ( "margin-left", "7px" )
              , ( "width", "28px")
              , ( "cursor", "default" )
              , ( "font-size", "12px")]]
              [text (if evaluate then (roundFloatString (-1 * evaluation)) else "")] in
  div [style
        [ ( "width", "160px" )
        , ( "height", "20px" )
        , ( "margin", "0 auto" )
        , ( "border-bottom", "1px solid black" )
        , ( "float", "bottom")
        ]] 
      [eval_button, ev, tog_ai_button, tog_depth_button]

drawHistory : (List (Color, Piece, Square, Square, (Maybe Piece), Maybe Square)) -> Int -> Bool -> Bool -> Float -> Bool -> Html Msg
-- draws the history box along with both controls, and evaluation
drawHistory mvs depth evaluate ai evaluation paused = 
  let len_mvs = List.length mvs in
  let toHist i (p, pc, sq_src, sq_dst, mpc, check) =
    div [ attribute "class" "square" 
           , style [ ( "height", "15px" )
                   , ( "width", "65px")
                   , ( "text-align", "center" )
                   , ( "vertical-align", "top" )
                   , ( "display" , "inline-block")
                   , ( "cursor", "pointer" )
                   , ( "margin-top", "6px")
                   , ( "font-size", "15px")]
            , onClick (MvHistory i)]
            [div [] [text ((Basics.toString ((len_mvs-i+1)//2)) ++ "." ++ (Piece.toString pc p) ++ 
                           (if (isJust mpc) then "x" else " ") ++ 
                           (Square.toStr sq_dst) ++ (if (isJust check) then "+" else "")) ]] in
  mvs |> List.indexedMap toHist|> List.reverse |> (++) [(drawMoveHistoryControls True)]
      |> (++) [(drawControls paused)] |> (++) [(drawEval depth evaluate ai evaluation)] |> (++) [drawFenControls]
        |> div [style
          [ ( "width", "160px" )
          , ( "height", "444px" )
          , ( "border", "1px solid black" )
          , ( "overflow-y", "scroll")
          , ( "overflow-x", "hidden")
          , ( "display" , "inline-block")
          , ( "vertical-align", "top" )
          ]] 
