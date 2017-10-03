module Game exposing(..)

import Piece exposing(..)
import PColor exposing(..)
import Matrix exposing(..)
import Board exposing(..)
import Square exposing(..)

type Dir = N | S | E | W | NE | NW | SE | SW
--type for direction (8 cardinal directions)
type alias CastleBools = {wsc : Bool, wlc : Bool, bsc : Bool, blc : Bool}
--alias for a record that stores castling information
type alias KingPoss = {w : Square, b : Square}

type alias MoveHist = (Color,Piece,Square,Square,Maybe Piece,Maybe Square)

justGet : Maybe a -> String -> a
--returns a maybe if just, else raises error 
justGet ma s =
  case ma of
    Just a -> a
    _      -> Debug.crash s

       
isJust : Maybe a -> Bool
--checks if a maybe is a just or a nothing
isJust m =
     case m of
        Just _ -> True
        _      -> False

takenth : List a -> Int -> Maybe a
--returns just of nth element of list if exists, else nothing
takenth xs i =
    if (i==0) then (List.head xs) else
        case xs of
            x::xs -> takenth xs (i-1)
            []    -> Nothing

walk : Dir -> Square -> Square
--increments location in one of the cardinal directions
walk d s =
    let (r,c) = s
    in case d of
        N  -> (r, c+1)
        S  -> (r, c-1)
        E  -> (r+1,c)
        W  -> (r-1,c)
        NE -> (r+1,c+1)
        NW -> (r-1,c+1)
        SE -> (r+1,c-1)
        SW -> (r-1,c-1)

canAtt : Dir -> List Piece 
-- given a direction, returns the pieces that can attack along that direction
canAtt d = 
    case d of
        N  -> [R,Q]
        S  -> [R,Q]
        E  -> [R,Q]
        W  -> [R,Q]
        _  -> [B,Q]

walkUntil : Board -> Color -> Square -> Dir -> (List Square)
--walks in given cardinal direction until possible moves exhausted
--(if either out of board bounds, blocked by another piece, or encounters friendly piece)
walkUntil b p s d = 
    let step = walk d s
    in if (not <| isValid step) then []
       else (if (isOccupied b step) then
            (if (isEnemy b p step) then step::[] else [])
            else step::(walkUntil b p step d))

walkUntilAtt : Board -> Color -> Square -> Dir -> Bool
--walks in given cardinal direction until possible moves exhausted
-- and checks if appropriate enemy piece lies at the end
walkUntilAtt b p s d = 
    let step = walk d s in
    if (not <| isValid step) then False
    else case (Matrix.get step b) of
        Just (NotEmpty pl pc) -> if (pl == p) then False else List.member pc (canAtt d)
        _                     -> walkUntilAtt b p step d

checkAttsDirH : Board -> Color -> Square -> (List Dir) -> Bool
checkAttsDirH b p s ds =
    case ds of 
        d::ds -> if (walkUntilAtt b p s d) then True else (checkAttsDirH b p s ds)
        _     -> False

checkAttsDir : Board -> Color -> Square -> Bool
checkAttsDir b p s =
    let dirs = [N, S, E, W, NE, NW, SE, SW] in (checkAttsDirH b p s dirs)

isEnemyPiece : Board -> Color -> Square -> Piece -> Bool
isEnemyPiece b p s pc =
    if (not (isValid s)) then False else
    case (Matrix.get s b) of
        Just (NotEmpty o_pl o_pc) -> if (o_pl == p) then False else (o_pc == pc)
        _                         -> False

checkAttsPawn : Board -> Color -> Square -> Bool
checkAttsPawn b p s =
    let (r,c) = s in
    let (sq_1,sq_2) = if (p == White) then ((r+1,c+1), (r+1,c-1)) else ((r-1,c+1), (r-1,c-1)) in
    (isEnemyPiece b p sq_1 P) || (isEnemyPiece b p sq_2 P)

checkAttsKn : Board -> Color -> Square -> Bool
checkAttsKn b p s = 
    let (r,c) = s in
    let poss_sqs = [(r+2,c+1),(r+1,c+2),(r+2,c-1),(r+1,c-2),
                    (r-2,c+1),(r-1,c+2),(r-2,c-1),(r-1,c-2)] in
    (checkAttsKnH b p poss_sqs)

checkAttsKnH : Board -> Color -> (List Square) -> Bool
checkAttsKnH b p sqs =
    case sqs of 
        sq::sqs -> if (isValid sq)&&(isEnemyPiece b p sq Kn) then True 
                    else (checkAttsKnH b p sqs)
        _       -> False

checkAttsKngH : Board -> Color -> (List Square) -> Bool
checkAttsKngH b p sqs =
    case sqs of 
        sq::sqs -> if (isValid sq)&&(isEnemyPiece b p sq K) then True 
                    else (checkAttsKngH b p sqs)
        _       -> False

checkAttsKng : Board -> Color -> Square -> Bool
checkAttsKng b p s =
    let (r,c) = s in
    let poss_sqs = [(r+1,c),(r-1,c),(r,c-1),(r,c+1),
                    (r+1,c-1),(r+1,c+1),(r-1,c-1),(r-1,c+1)] in
    (checkAttsKngH b p poss_sqs)

sqInAttackEfficient : Board -> Color -> Square -> Bool
sqInAttackEfficient b p sq =
    if (checkAttsPawn b p sq) then True else
    if (checkAttsKn b p sq) then True else
    if (checkAttsKng b p sq) then True else
    if (checkAttsDir b p sq) then True else False

isOccupied : Board -> Square -> Bool
--checks if square on board is occupied
isOccupied b s = 
    case (Matrix.get s b) of
        Just (NotEmpty _ _) -> True
        _                   -> False 

isOccupiedByPl : Board -> Color -> Square -> Bool
--checks if square on board is occupied by given player
isOccupiedByPl b p s = 
    case (Matrix.get s b) of
        Just (NotEmpty pl _) -> (p == pl)
        _                    -> False 


isEnemy : Board -> Color -> Square -> Bool
--checks if square on board is enemy (enemy is any piece not occupied by you)
isEnemy b p s =
    case (Matrix.get s b) of
        Just (NotEmpty c _) -> p /= c
        Just _        -> True -- an empty is an enemy position (in that it is not yours)
                              -- this contrivance makes checking valid moves a little easier
        _             -> False -- if outside range, then not an enemy

rookDirs : List Dir
--dirs that rook can move in
rookDirs = [N, S, E, W]

bishopDirs : List Dir
--dirs that bishop can move in
bishopDirs = [NE, NW, SE, SW]

queenDirs : List Dir
--dirs that queen can move in
queenDirs = [N, S, E, W, NE, NW, SE, SW]

kingMovesWithCastle : Board -> Color -> Square -> Bool -> Bool -> (List Square)
--returns list of all possible king moves (including castle moves)
kingMovesWithCastle b p s sc lc =
    let (r,c) = s in
    (kingMoves b p s) 
    ++ (if (sc && (not ((isOccupied b (r,c+1))||(isOccupied b (r,c+2))||
                        (sqInAttackEfficient b p (r,c))||(sqInAttackEfficient b p (r,c+1))
                        ||(sqInAttackEfficient b p (r,c+2))))) 
        then [(r,c+2)] else [])
    ++ (if (lc && (not ((isOccupied b (r,c-1))||(isOccupied b (r,c-2))||(isOccupied b (r,c-3))||
                        (sqInAttackEfficient b p (r,c))||(sqInAttackEfficient b p (r,c-1))
                        ||(sqInAttackEfficient b p (r,c-2))||(sqInAttackEfficient b p (r,c-3))))) 
        then [(r,c-2)] else [])

kingMoves : Board -> Color -> Square -> (List Square)
--returns list of possible non-castle moves
kingMoves b p s =
    let (r,c) = s in
    let sqs = [(r,c+1),(r,c-1),(r+1,c),(r-1,c),(r+1,c+1),
               (r-1,c+1),(r+1,c-1),(r-1,c-1)] in
    sqs |> List.filter isValid |> List.filter (isEnemy b p) -- filters out squares that are not enemy (so occupied by you)

knightMoves : Board -> Color -> Square -> (List Square)
--returns list of possible knight moves
knightMoves b p s =
    let (r,c) = s in
    let sqs = [(r+2,c+1),(r+1,c+2),(r+2,c-1),(r+1,c-2),
               (r-2,c+1),(r-1,c+2),(r-2,c-1),(r-1,c-2)] in
    sqs |> List.filter isValid |> List.filter (isEnemy b p) -- filters out squares that are not enemy (so occupied by you)

pawnMoves : Board -> Color -> Square -> (Maybe Int) -> (List Square)
--returns list of possible pawn moves (including promotion moves, and capture moves, but not en-passant)
pawnMoves b p s mi =
    let sgn = if (p == Black) then -1 else 1 in
    let (r,c) = s in
    let captureMove s = if ((isOccupied b s) && (isEnemy b p s)) then [s] else [] in
    let nonCaptureMove s = if (isOccupied b s) then [] else [s] in
    let firstRankMove = if (((pawnRank p) == r) && (not ((isOccupied b (r+(sgn*2),c))
                                                       ||(isOccupied b (r+(sgn*1),c))))) then 
        [(r+(sgn*2),c)] else [] in
    let enpassrank = if (p == White) then 4 else 3 in
    let enPassMove = 
        case mi of
            Just i -> if ((s == (enpassrank,i-1))||(s == (enpassrank,i+1))) then [(r+1,i)] else []
            _      -> [] in
    (nonCaptureMove (r+(sgn*1),c)) ++ (captureMove (r+(sgn*1),c-1)) ++ (captureMove (r+(sgn*1),c+1)) 
    ++ firstRankMove ++ enPassMove

pawnAttacks : Board -> Color -> Square -> (List Square)
--returns list of squares attacked by a pawn 
pawnAttacks b p s =
    let sgn = if (p == Black) then -1 else 1 in
    let (r,c) = s in
    let captureMove s = if ((isOccupied b s) && (isEnemy b p s)) then [s] else [] in
    (captureMove (r+(sgn*1),c-1)) ++ (captureMove (r+(sgn*1),c+1))

pieceDirMoves : Board -> Color -> Square -> (List Dir) -> (List Square)
--returns list of possible moves given a list of directions (for queen, rook and bishop)
pieceDirMoves b p s ds =
    List.concatMap (walkUntil b p s) ds

pieceMovesWithoutLegality : Board -> CastleBools -> Color -> Square -> Piece -> (Maybe Int) -> (List Square)
--returns all legal piece moves, (all possible moves that don't leave king in check)
pieceMovesWithoutLegality b cbs p s pc mi =
    let sc = if (p == White) then cbs.wsc else cbs.bsc in
    let lc = if (p == White) then cbs.wlc else cbs.blc in
        case pc of
            K  -> kingMovesWithCastle b p s sc lc
            Q  -> pieceDirMoves b p s queenDirs
            R  -> pieceDirMoves b p s rookDirs
            B  -> pieceDirMoves b p s bishopDirs
            Kn -> knightMoves b p s
            P  -> pawnMoves b p s mi

pieceMoves : Board -> CastleBools -> Color -> Square -> Piece -> (Maybe Int) -> KingPoss -> (List Square)
--returns all legal piece moves, (all possible moves that don't leave king in check)
pieceMoves b cbs p s pc mi kposs =
    let sc = if (p == White) then cbs.wsc else cbs.bsc in
    let lc = if (p == White) then cbs.wlc else cbs.blc in
    let poss_mvs = pieceMovesWithoutLegality b cbs p s pc mi in
    let test_mv sq =
        let (new_b,new_kposs) = (makeMoveSimple b cbs s sq kposs) in
        let k_sq = if (p == White) then new_kposs.w else new_kposs.b in
        not (sqInAttackEfficient new_b p k_sq) in -- test passed if king is not in attack after making move
    poss_mvs |> List.filter test_mv

pieceAttacks : Board -> Color -> Square -> Piece -> (List Square)
--return list of squares attacked by given piece (special case for pawn and king)
pieceAttacks b p s pc = -- squares attacked by pc at s belonging to s
    case pc of
        K  -> kingMoves b p s
        Q  -> pieceDirMoves b p s queenDirs
        R  -> pieceDirMoves b p s rookDirs
        B  -> pieceDirMoves b p s bishopDirs
        Kn -> knightMoves b p s
        P  -> pawnAttacks b p s 

sqMoves : Board -> CastleBools -> Square -> (Maybe Int) -> KingPoss -> (List Square)
--given a square, returns all possible moves of that square
sqMoves b cbs s mi kposs =
    case (Matrix.get s b) of
        Just (NotEmpty p pc) -> pieceMoves b cbs p s pc mi kposs
        _                    -> [] 

sqMovesWithoutLegality : Board -> CastleBools -> Square -> (Maybe Int) -> (List Square)
--given a square, returns all possible moves of that square
sqMovesWithoutLegality b cbs s mi =
    case (Matrix.get s b) of
        Just (NotEmpty p pc) -> pieceMovesWithoutLegality b cbs p s pc mi
        _                    -> [] 

makeEnPassMove : Board -> CastleBools -> Square -> Square -> KingPoss -> (Board, CastleBools, Maybe Piece, KingPoss, Maybe Int, MoveHist)
makeEnPassMove b cbs sq_src sq_dest kposs =
    let (r_dest, c_dest) = sq_dest in
    let (r_src, c_src) = sq_src in
    let capt_sq = (r_src,c_dest) in
    let (p, pc)  = (case (justGet (Matrix.get sq_src b) "makeEnPassMove - impossible") of
                        NotEmpty a b -> (a,b)
                        _            -> Debug.crash "makeEnPassMove - impossible")  in 
    let captured = (case (justGet (Matrix.get capt_sq b) "makeEnPassMove - impossible") of
                        NotEmpty a b -> Just b
                        Empty        -> Debug.crash "makeEnPassMove - impossible")  in 
    let rm_pc_b  = Matrix.set sq_src Empty b in
    let mv_pc_b  = Matrix.set sq_dest (NotEmpty p pc) rm_pc_b in
    let rm_capt_b = Matrix.set capt_sq Empty mv_pc_b in
    let check = (kingInCheck rm_capt_b (PColor.otherColor p)) in
    let srm = Nothing in
    (rm_capt_b, cbs, captured, kposs, srm, (p, pc, sq_src, sq_dest, captured, check))



makeMove : Board -> CastleBools -> Square -> Square -> KingPoss -> (Board, CastleBools, Maybe Piece, KingPoss, Maybe Int, MoveHist)
--given a board state, castles state and a source and destination, performs the move (raises error if illegal)
--and returns updated board, castles state, maybe a captured piece, and a tuple of the move (stored in model)
makeMove b cbs sq_src sq_dest kposs =
    let (r_dest, c_dest) = sq_dest in
    let (r_src, c_src) = sq_src in
    let (p, pc)  = (case (justGet (Matrix.get sq_src b) "makeMove - impossible") of
                        NotEmpty a b -> (a,b)
                        _            -> Debug.crash "makeMove - impossible")  in 
    if ((pc == P)&&(c_src/=c_dest)&&(not (isOccupied b sq_dest))) then (makeEnPassMove b cbs sq_src sq_dest kposs) else
    let captured = (case (justGet (Matrix.get sq_dest b) "makeMove - impossible") of
                        NotEmpty a b -> Just b
                        Empty        -> Nothing)  in 
    let rm_pc_b  = Matrix.set sq_src Empty b in
    let mv_pc_b  = Matrix.set sq_dest (NotEmpty p pc) rm_pc_b in
    let prom_b   = if ((pc == P) && (r_dest%7==0)) 
                   then Matrix.set sq_dest (NotEmpty p Q) mv_pc_b
                   else mv_pc_b in
    let castle_b = if (pc == K) then
                        if ((c_src - c_dest) == -2) --short castle
                            then prom_b |> Matrix.set (r_src, 7) Empty
                                         |> Matrix.set (r_src, 5) (NotEmpty p R)
                        else (if ((c_src - c_dest) == 2) --long castle
                            then prom_b |> Matrix.set (r_src, 0) Empty
                                        |> Matrix.set (r_src, 3) (NotEmpty p R)
                        else prom_b)
                    else prom_b in
    let wsc_n = cbs.wsc && (sq_src /= (0,7)) && (sq_src /= (0,4)) in
    let wlc_n = cbs.wlc && (sq_src /= (0,0)) && (sq_src /= (0,4)) in
    let bsc_n = cbs.bsc && (sq_src /= (7,7)) && (sq_src /= (7,4)) in
    let blc_n = cbs.blc && (sq_src /= (7,0)) && (sq_src /= (7,4)) in
    let check = (kingInCheck castle_b (PColor.otherColor p)) in
    let srm = if ((pc == P)&&(Basics.abs(r_dest - r_src) == 2)) then Just c_src else Nothing in
    let kposs_new = if (pc == K) then (if (p == White) then {kposs | w = sq_dest} else {kposs | b = sq_dest}) else kposs in
    (castle_b, {wsc=wsc_n,wlc=wlc_n,bsc=bsc_n,blc=blc_n}, captured, kposs_new, srm, (p, pc, sq_src, sq_dest, captured, check))

makeMoveSimple : Board -> CastleBools -> Square -> Square -> KingPoss -> (Board, KingPoss)
-- for testing valid moves
makeMoveSimple b cbs sq_src sq_dest kposs =
    let (r_dest, c_dest) = sq_dest in
    let (r_src, c_src) = sq_src in
    let (p, pc)  = (case (justGet (Matrix.get sq_src b) "makeMove - impossible") of
                        NotEmpty a b -> (a,b)
                        _            -> Debug.crash "makeMove - impossible")  in 
    let rm_pc_b  = Matrix.set sq_src Empty b in
    let mv_pc_b  = Matrix.set sq_dest (NotEmpty p pc) rm_pc_b in
    let prom_b   = if ((pc == P) && (r_dest%7==0)) 
                   then Matrix.set sq_dest (NotEmpty p Q) mv_pc_b
                   else mv_pc_b in
    let castle_b = if (pc == K) then
                        if ((c_src - c_dest) == -2) --short castle
                            then prom_b |> Matrix.set (r_src, 7) Empty
                                         |> Matrix.set (r_src, 5) (NotEmpty p R)
                        else (if ((c_src - c_dest) == 2) --long castle
                            then prom_b |> Matrix.set (r_src, 0) Empty
                                        |> Matrix.set (r_src, 3) (NotEmpty p R)
                        else prom_b)
                    else prom_b in
    let kposs_new = if (pc == K) then (if (p == White) then {kposs | w = sq_dest} else {kposs | b = sq_dest}) else kposs in
    (castle_b, kposs_new)


inAttack : Board -> Square -> Color -> Square -> Bool
--checks if given sq is under threat from another square
inAttack b spc p s = -- is pc at s (owned by p) in attack by pc at spc
    case (Matrix.get spc b) of
        Just (NotEmpty ppc pc) -> 
            if (ppc == p) then False else 
            pieceAttacks b ppc spc pc |> List.member s
        _                   -> False 

kingInCheck : Board -> Color -> (Maybe Square)
--checks if the king of given color is under threat, return just the square of the king if so
kingInCheck b p =
    let k_sq = findKing b p in
    if (sqInAttackEfficient b p k_sq) then Just k_sq else Nothing 

findKing : Board -> Color -> Square
--finds the king of a certain color
findKing b pl =
    let l_loc_b = b |> Matrix.mapWithLocation (\loc a-> (loc,a)) |> Matrix.flatten in
    let red_l l = case l of
        []                       -> Debug.crash ("impossible, no King - findKing " ++ 
                                    (boardToString b))
        (loc,(NotEmpty p pc))::l -> if ((p == pl) && (pc == K)) then loc else red_l l
        _::l                     -> red_l l in
     red_l l_loc_b

hasKing : Board -> Color -> Bool
--checks if given color has king (for debugging)
hasKing b pl =
    let l_b = b |> Matrix.flatten in
    let red_l l = case l of
        []                       -> False
        (NotEmpty p pc)::l -> if ((p == pl) && (pc == K)) then True else red_l l
        _::l                     -> red_l l in
     red_l l_b

sqInAttackTimes : Board -> Color -> Square -> Int
--checks how many times square is under attack from any other square
sqInAttackTimes b p k_sq =
    let locs = b |> Matrix.mapWithLocation (\loc a-> loc) |> Matrix.flatten in
    locs |> List.foldr (\sq acc -> acc+(if (inAttack b sq p k_sq) then 1 else 0)) 0

validSquares : Board -> CastleBools -> Color -> Maybe Int -> KingPoss -> (List Square)
--returns list of all possible valid squares in a state (used for evaluation, and checking check-mate/stalemate)
validSquares b cbs p mi kposs =
    let locs = b |> Matrix.mapWithLocation (\loc a-> loc) |> Matrix.flatten in
    let tmpSqMvs sq acc = 
        case (Matrix.get sq b) of
            Just (NotEmpty sq_p sq_pc) -> let new_mvs = if (p==sq_p) then (sqMoves b cbs sq mi kposs) else [] in
                                          new_mvs++acc
            _                          -> acc in
    locs |> List.foldr tmpSqMvs []

validMoves : Board -> CastleBools -> Color -> Maybe Int -> KingPoss -> (List (Square, Square))
--returns list of all possible valid moves in a state (used for evaluation, and checking check-mate/stalemate)
validMoves b cbs p mi kposs =
    let locs = b |> Matrix.mapWithLocation (\loc a-> loc) |> Matrix.flatten in
    let tmpSqMvs sq acc = 
        case (Matrix.get sq b) of
            Just (NotEmpty sq_p sq_pc) -> let new_mvs = if (p==sq_p) 
                                          then (List.map (\s -> (sq,s)) (sqMoves b cbs sq mi kposs)) else [] in
                                          new_mvs++acc
            _                          -> acc in
    locs |> List.foldr tmpSqMvs []

validSquaresWithoutLegality : Board -> CastleBools -> Color -> (Maybe Int)-> (List Square)
--returns list of all possible valid squares in a state (used for evaluation, and checking check-mate/stalemate)
validSquaresWithoutLegality b cbs p mi =
    let locs = b |> Matrix.mapWithLocation (\loc a-> loc) |> Matrix.flatten in
    let tmpSqMvs sq acc = 
        case (Matrix.get sq b) of
            Just (NotEmpty sq_p sq_pc) -> let new_mvs = if (p==sq_p) then (sqMovesWithoutLegality b cbs sq mi) else [] in
                                          new_mvs++acc
            _                          -> acc in
    locs |> List.foldr tmpSqMvs []
