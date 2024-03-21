(* Copyright (C) 2015-2023
   Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE chessBoard ;  (*m2iso+gm2*)

FROM libc IMPORT printf, exit, sleep, getpid ;
FROM wrapc IMPORT getrand ;
FROM SYSTEM IMPORT BYTE, WORD, WORD16, INTEGER8, CARDINAL8, ADDRESS ;
FROM Storage IMPORT ALLOCATE ;
FROM Assertion IMPORT Assert ;
FROM PushBackInput IMPORT Open, PutCh, GetCh, Error ;
FROM FIO IMPORT File, StdIn ;
FROM Indexing IMPORT Index, HighIndice, PutIndice, GetIndice, InitIndex, InBounds ;
FROM StringConvert IMPORT stoc, ctos ;
FROM SCmdArgs IMPORT GetArg ;
FROM ASCII IMPORT cr, lf, tab ;

FROM DynamicStrings IMPORT String, string, InitString, KillString, Equal, ConCatChar,
                           char, Length, EqualArray, ConCat, ConCatChar ;

IMPORT multiprocessor, colors ;
FROM multiprocessor IMPORT SEMAPHORE, wait, signal ;
IMPORT mailbox, m2config ;
FROM mailbox IMPORT Mailbox ;


CONST
   DebugAlphaBeta  = FALSE ;
   DebugGeneration = FALSE ;
   DebugMicroCode  = FALSE ;
   Stress          =  TRUE ;
   MaxStack        =      20 ;
   MaxPlies        =      20 ;
   MaxScore        =  100000 ;
   MinScore        = -100000 ;
   WhiteWin        = MaxScore - MaxPlies ;
   BlackWin        = MinScore + MaxPlies ;
   MaxSquares      = 64 ;
   BoardSize       =  8 ;
   MaxHeap         = 1000H * 64 ; (* the instruction heap.  *)
   MaxPlyMoves     = 1000 ;       (* the maximum number of moves possible on a ply.  *)
   MaxMoves        = 1000 * 64 ;  (* the move heap, maximum number of moves stored  *)
                                  (* in the tree.  Maximum necessary for depth first.  *)
   CornerBotLeft   = 0 ;
   CornerBotRight  = 7 ;
   CornerTopLeft   = 56 ;
   CornerTopRight  = 63 ;
   WhiteKing       = 3 ;          (* white king starts on this square.  *)
   BlackKing       = 59 ;         (* black king starts on this square.  *)
   debugging       = FALSE ;
   debugMultiProc  = TRUE ;       (* display multiprocessor debugging.  *)
   UnusedSquare    = MaxSquares ; (* if this square is indexed then the piece is empty.  *)
   DefaultPlies    = 3 ;          (* default to looking 3 plies ahead.  *)
   unusedPlyHeap   = 0 ;
   MaxPlyPool      = 100 ;
   MaxProcessors   =  64 ;
   ScalePiece      =  256 ;       (* 8 bits needed for the search flags and ply value.  *)
   ScalePly        =    8 ;       (* 3 bits needed to encode the result.  After that goes the ply value and then the material score.  *)

TYPE
   status = (finished, ready, running) ;

   processorInfo = RECORD
                      childPid: INTEGER ;
                      result  : INTEGER ;
                      move    : MoveRange ;
                      pstatus : status ;
                   END ;

   plyProcessor = RECORD
                     childrenActive      : CARDINAL ;
                     topCount            : CARDINAL ;  (* field only used by top.  *)
                     initialised,                      (* have the semaphores been initialised yet?  *)
                     inuse               : BOOLEAN ;   (* is the record currently in use?  *)
                     plyTurn             : Colour ;
                     plyAlpha,
                     plyBeta,
                     best                : INTEGER ;   (* current best score found.  *)
                     bestMove            : MoveRange ;
                     plyMutex            : SEMAPHORE ;
                     barrier             : Mailbox ;
                     nextFree            : CARDINAL ;  (* next available plyProcessor.  *)
                     isparallel          : BOOLEAN ;   (* use the parallel algorithm?  *)
                  END ;

   MoveRange       = [0..MaxMoves] ;   (* index into the moves array.  *)

   DecodeProcedure = PROCEDURE (VAR Board, MoveRange, plyRange) ;

   Colour          = (white, black) ;
   PieceT          = (none, pawn, shadow, knight, bishop, rook, queen, king) ;
   Squares         = [0..MaxSquares-1] <* bytealignment (0) *> ;
   PieceIndex      = [0..MAX (KingRange)] <* bytealignment (0) *> ;

   PawnRange       = [0..7] <* bytealignment (0) *> ;
   ShadowRange     = [8..8] <* bytealignment (0) *> ;
   KnightRange     = [9..18] <* bytealignment (0) *> ;
   WBishopRange    = [19..27] <* bytealignment (0) *> ;
   BBishopRange    = [28..36] <* bytealignment (0) *> ;
   RookRange       = [37..46] <* bytealignment (0) *> ;
   QueenRange      = [47..55] <* bytealignment (0) *> ;
   KingRange       = [56..56] <* bytealignment (0) *> ;
   NoneRange       = [57..57] <* bytealignment (0) *> ;   (* empty piece.  *)

   castling = (bks, bqs, wks, wqs) ;

   cFlags = SET OF castling ;

   Board = RECORD
              pieces    : ARRAY Colour OF Pieces ;
              square    : ARRAY Squares OF Square ;
              noTake    : CARDINAL8 ;  (* how many moves since we last took a piece?  *)
              fullMoveNo: CARDINAL8 ;  (* which full move number are we on?   *)
              cflags    : cFlags ;
              score     : INTEGER ;
           END ;

   Square = RECORD
               <* bytealignment (0) *>
               pix   : PieceIndex ;
               used  : BOOLEAN ;
               colour: Colour ;
            END ;

   Pieces = RECORD
               nPawns     : CARDINAL8 ;
               pawn       : ARRAY [0..BoardSize-1] OF Pawn ;
               nShadowPawn: CARDINAL8 ;
               shadowPawn : Pawn ;
               nKnights   : CARDINAL8 ;
               knight     : ARRAY [0..BoardSize+1] OF Knight ;
               nWBishops  : CARDINAL8 ;
               wbishop    : ARRAY [0..BoardSize] OF WBishop ;
               nBBishops  : CARDINAL8 ;
               bbishop    : ARRAY [0..BoardSize] OF BBishop ;
               nRooks     : CARDINAL8 ;
               rook       : ARRAY [0..BoardSize+1] OF Rook ;
               nQueens    : CARDINAL8 ;
               queen      : ARRAY [0..BoardSize+1] OF Queen ;
               king       : King ;
            END ;

   Pawn = RECORD
             xy: CARDINAL8 ;
          END ;

   Knight = RECORD
               xy: CARDINAL8 ;
            END ;

   WBishop = RECORD
                xy: CARDINAL8 ;
             END ;

   BBishop = RECORD
                xy: CARDINAL8 ;
             END ;

   Rook = RECORD
             xy: CARDINAL8 ;
          END ;

   Queen = RECORD
             xy: CARDINAL8 ;
           END ;

   King = RECORD
             xy     : CARDINAL8 ;
          END ;

   HeapRange = [0..MaxHeap] ;

   Heap = ARRAY HeapRange OF WORD16 ;

   NoTakeT = [0..50] ;

   HeapOpcode = (move, add, sub, flags) ;

   Instruction     = RECORD
                         <* bytealignment(0) *>
                         last     : BOOLEAN ;     (* 1 bit (first bit).  *)
                         opcode   : HeapOpcode ;  (* 2 bits *)
                         padding  : BYTE ;        (* forces this record to a 16 bit unit.  *)
                     END ;

   InstructionAdd  = RECORD
                        <* bytealignment(0) *>
                        last     : BOOLEAN ;     (* 1 bit  *)
                        opcode   : HeapOpcode ;  (* 2 bits *)
                        colour   : Colour ;      (* 1 bit  *)
                        pieceno  : PieceIndex ;  (* 6 bits *)
                        xy       : Squares ;     (* 6 bits *)
                     END ;

   InstructionSub  = RECORD
                        <* bytealignment(0) *>
                        last     : BOOLEAN ;     (* 1 bit  *)
                        opcode   : HeapOpcode ;  (* 2 bits *)
                        xy       : Squares ;     (* 6 bits *)
                     END ;

   InstructionMove = RECORD
                        <* bytealignment(0) *>
                        last     : BOOLEAN ;     (* 1 bit  *)
                        opcode   : HeapOpcode ;  (* 2 bits *)
                        pieceno  : PieceIndex ;  (* 6 bits *)
                        colour   : Colour ;      (* 1 bit  *)
                        xy       : Squares ;     (* 6 bits *)
                     END ;

   InstructionFlags = RECORD
                          <* bytealignment(0) *>
                          last     : BOOLEAN ;     (* 1 bit  *)
                          opcode   : HeapOpcode ;  (* 2 bits *)
                          bccqs,
                          bccks,
                          wccks,
                          wccqs    : BOOLEAN ;     (* 4 bits *)
                          setNoTake: BOOLEAN ;     (* 1 bit  *)
                          noTake   : NoTakeT ;     (* 6 bits *)
                      END ;

   KindType = (mov, tak, nop) ;

   MoveDescriptor = RECORD
                       kind  :  KindType ;
                       decons,
                       cons  :  HeapRange ;
                    END ;

   FrameDescriptor = RECORD
                        heapPtr:  HeapRange ;
                        movePtr:  MoveRange ;
                     END ;

   SearchFlag = (forcewhitewin, forceblackwin, forcedraw, validsearch) ;
   SearchFlagSet = SET OF SearchFlag ;

   PieceValue = ARRAY PieceT OF INTEGER ;

   plyRange = [0..MaxPlyPool] ;

   plyArrayRange = [1..MaxPlyPool] ;

   plyArray = ARRAY plyArrayRange OF plyProcessor ;

VAR
   heapPtr            : HeapRange ;
   takePtr,
   movePtr            : MoveRange ;
   heap               : Heap ;
   currentBoard       : Board ;
   turn               : Colour ;
   timeAllowed        : CARDINAL ;   (* maxtime per move for the computer.  *)
   boardNo            : CARDINAL ;
   inputFile,
   outputFile         : File ;
   limitPlies,
   prioritiseTime,
   verbose,
   debug,
   groff,
   verify             : BOOLEAN ;
   history            : Index ;
   moves              : ARRAY MoveRange OF MoveDescriptor ;
   usedProcessors,
   maxProcessors      : CARDINAL ;
   topFrame           : FrameDescriptor ;
   totalMoveCount     : CARDINAL ;  (* count of moves explored in one turn.  *)
   maxPlies           : CARDINAL ;  (* how deep should the alphaBeta alg descend.  *)
   stackString        : ARRAY [0..MaxStack] OF String ;
   stackPtr           : CARDINAL ;
   searchFlags        : SearchFlagSet ;
   pieceValue         : PieceValue ;
   plyPool            : POINTER TO plyArray ;
   topPoolId,
   freePlyPool        : CARDINAL ;
   mutexPlyPool       : SEMAPHORE ;
   processorAvailable : SEMAPHORE ;
   processors         : ARRAY [0..MaxPlyMoves] OF processorInfo ;


(*
   initPlyPool -
*)

PROCEDURE initPlyPool (i: CARDINAL; noProcessors: CARDINAL;
                       turn: Colour; alpha, beta: INTEGER; parallel: BOOLEAN) : CARDINAL ;
BEGIN
   WITH plyPool^[i] DO
      IF NOT initialised
      THEN
         initialised := TRUE ;
         IF m2config.MULTIPROCESSOR
         THEN
            plyMutex := multiprocessor.initSem (1) ;
            barrier := mailbox.init ()
         END
      END ;
      childrenActive := 0 ;
      topCount := 0 ;
      stop ;
      plyAlpha := alpha ;
      plyBeta := beta ;
      plyTurn := turn ;
      best := minScoreColour (turn) ;
      bestMove := MAX (MoveRange) ;
      inuse := TRUE ;
      isparallel := parallel ;
   END ;
   RETURN i
END initPlyPool ;


(*
   newPlyPool - return a new plyProcessor descriptor.
*)

PROCEDURE newPlyPool (noProcessors: CARDINAL; turn: Colour; alpha, beta: INTEGER;
                      parallel: BOOLEAN) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   IF m2config.MULTIPROCESSOR
   THEN
      wait (mutexPlyPool)
   END ;
   i := freePlyPool ;
   IF i = 0
   THEN
      HALT  (* increase MaxPlyPool.  *)
   END ;
   freePlyPool := plyPool^[i].nextFree ;
   Assert (NOT plyPool^[i].inuse) ;
   i := initPlyPool (i, noProcessors, turn, alpha, beta, parallel) ;
   IF m2config.MULTIPROCESSOR
   THEN
      signal (mutexPlyPool)
   END ;
   RETURN i
END newPlyPool ;


(*
   killPlyPool -
*)

PROCEDURE killPlyPool (poolId: CARDINAL) : CARDINAL ;
BEGIN
   Assert (poolId # 0) ;
   IF m2config.MULTIPROCESSOR
   THEN
      wait (mutexPlyPool)
   END ;
   plyPool^[poolId].inuse := FALSE ;
   plyPool^[poolId].nextFree := freePlyPool ;
   freePlyPool := poolId ;
   IF m2config.MULTIPROCESSOR
   THEN
      signal (mutexPlyPool)
   END ;
   RETURN unusedPlyHeap
END killPlyPool ;


(*
   pushBoard -
*)

PROCEDURE pushBoard (s: String) ;
BEGIN
   stackString[stackPtr] := s ;
   INC (stackPtr)
END pushBoard ;


(*
   popBoard - returns TRUE if s matches the stacked string.
*)

PROCEDURE popBoard (s: String) : BOOLEAN ;
VAR
   match: BOOLEAN ;
BEGIN
   DEC (stackPtr) ;
   match := Equal (s, stackString[stackPtr]) ;
   IF NOT match
   THEN
      printf ("popBoard verify failure between: %s and %s\n",
               string (s), string (stackString[stackPtr])) ;
   END ;
   stackString[stackPtr] := KillString (stackString[stackPtr]) ;
   s := KillString (s) ;
   RETURN match
END popBoard ;


(*
   assertPopBoard -
*)

PROCEDURE assertPopBoard (s: String; pre, post, b: Board; cons, decons: HeapRange) ;
BEGIN
   IF NOT popBoard (s)
   THEN
      printf ("board before the move\n") ;
      displayBoard (pre) ;
      printf ("cons microcode\n") ;
      dumpMicrocode (pre, cons, 0) ;
      printf ("\nboard after the move\n") ;
      displayBoard (post) ;
      printf ("decons microcode\n") ;
      dumpMicrocode (post, decons, 0) ;
      printf ("\nthe calculated board after the decons\n") ;
      displayBoard (b) ;
      HALT
   END
END assertPopBoard ;


(*
   saveCurrentBoard -
*)

PROCEDURE saveCurrentBoard ;
VAR
   p: POINTER TO Board ;
BEGIN
   NEW (p) ;
   p^ := currentBoard ;
   PutIndice (history, HighIndice (history)+1, p) ;
   printf ("board saved as board %d\n", HighIndice (history))
END saveCurrentBoard ;


(*
   inRange -
*)

PROCEDURE inRange (sq: CARDINAL8; dx, dy: INTEGER8; VAR nx, ny: INTEGER8) : BOOLEAN ;
BEGIN
   nx := sq MOD BoardSize ;
   ny := sq DIV BoardSize ;
   nx := nx + dx ;
   ny := ny + dy ;
   RETURN (nx>=0) AND (nx<BoardSize) AND (ny>=0) AND (ny<BoardSize)
END inRange ;


(*
   XY -
*)

PROCEDURE XY (x, y: INTEGER8) : CARDINAL8 ;
BEGIN
   RETURN y * BoardSize + x
END XY ;


(*
   genMoves -
*)

PROCEDURE genMoves (VAR b: Board; col: Colour) ;
BEGIN
   takePtr := movePtr ;   (* taking moves always end up at the start of the list.  *)
   moves[movePtr].kind := nop ;  (* first move is currently unassigned.  *)
   genPawnMoves (b, col) ;
   genKnightMoves (b, col) ;
   genBishopMoves (b, col) ;
   genRookMoves (b, col) ;
   genQueenMoves (b, col) ;
   genKingMoves (b, col) ;
   IF moves[takePtr].kind = nop
   THEN
      movePtr := takePtr
   END
END genMoves ;


(*
   getColour -
*)

PROCEDURE getColour (VAR b: Board; sq: INTEGER8) : Colour ;
BEGIN
   RETURN b.square[sq].colour
END getColour ;


(*
   isOpponentOccupied -
*)

PROCEDURE isOpponentOccupied (VAR b: Board; c: Colour; sq: INTEGER8) : BOOLEAN ;
BEGIN
   RETURN b.square[sq].used AND (getColour (b, sq)#c)
END isOpponentOccupied ;


(*
   excludeCastling -
*)

PROCEDURE excludeCastling (cflags: cFlags; col: Colour) : cFlags ;
BEGIN
   IF col = white
   THEN
      RETURN cflags - cFlags {wks, wqs}
   ELSE
      RETURN cflags - cFlags {bks, bqs}
   END ;
END excludeCastling ;


(*
   excludeCastlingRook -
*)

PROCEDURE excludeCastlingRook (cflags: cFlags; square: CARDINAL8) : cFlags ;
BEGIN
   IF square = CornerBotLeft
   THEN
      (* white left rook - king side rook.  *)
      RETURN cflags - cFlags {wks}
   ELSIF square = CornerBotRight
   THEN
      (* white right rook - queen side rook.  *)
      RETURN cflags - cFlags {wqs}
   ELSIF square = CornerTopLeft
   THEN
      (* black left rook - king side rook.  *)
      RETURN cflags - cFlags {bks}
   ELSIF square = CornerTopRight
   THEN
      (* black right rook - queen side rook.  *)
      RETURN cflags - cFlags {bqs}
   ELSE
      RETURN cflags
   END
END excludeCastlingRook ;


(*
   tryMovingKing -
*)

PROCEDURE tryMovingKing (VAR b: Board; col: Colour; pieceNo: CARDINAL8; dx, dy: INTEGER) ;
VAR
   to, sq: CARDINAL8 ;
   nx, ny: INTEGER8 ;
   cflags: cFlags ;
BEGIN
   sq := b.pieces[col].king.xy ;
   IF inRange (sq, dx, dy, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF isEmpty (b, to)
      THEN
         tryMovePiece (b, col, pieceNo, sq, to)
      ELSIF isOpponentOccupied (b, col, to)
      THEN
         tryMoveTakePiece (b, col, pieceNo, sq, to)
      END
   END
END tryMovingKing ;


(*
   genKingMoves -
*)

PROCEDURE genKingMoves (VAR b: Board; c: Colour) ;
BEGIN
   IF c = white
   THEN
      IF wks IN b.cflags
      THEN
         (* --fixme-- complete the castling tests.  *)
         (* need to test king/1 left/2 left for empty and not in check.  *)
      END ;
      IF wqs IN b.cflags
      THEN
         (* --fixme-- complete the castling tests.  *)
         (* need to test king/1 right/2 right/3 right for empty and not in check.  *)
      END
   ELSE
      IF bks IN b.cflags
      THEN
         (* --fixme-- complete the castling tests.  *)
         (* need to test king/1 left/2 left for empty and not in check.  *)
      END ;
      IF bqs IN b.cflags
      THEN
         (* --fixme-- complete the castling tests.  *)
         (* need to test king/1 right/2 right/3 right for empty and not in check.  *)
      END
   END ;
   tryMovingKing (b, c, MIN (KingRange), 1, 0) ;
   tryMovingKing (b, c, MIN (KingRange), 0, 1) ;
   tryMovingKing (b, c, MIN (KingRange), -1, 0) ;
   tryMovingKing (b, c, MIN (KingRange), 0, -1) ;
   tryMovingKing (b, c, MIN (KingRange), 1, 1) ;
   tryMovingKing (b, c, MIN (KingRange), -1, -1) ;
   tryMovingKing (b, c, MIN (KingRange), -1, 1) ;
   tryMovingKing (b, c, MIN (KingRange), 1, -1)
END genKingMoves ;


(*
   genRookMoves -
*)

PROCEDURE genRookMoves (VAR b: Board; colour: Colour) ;
VAR
   nRooks,
   r       : CARDINAL ;
   x, y    : INTEGER8 ;
BEGIN
   nRooks := b.pieces[colour].nRooks ;
   r := 0 ;
   WHILE r < nRooks DO
      IF b.pieces[colour].rook[r].xy < UnusedSquare
      THEN
         x := 1 ;
         WHILE tryMovingRook (b, colour, r, x, 0) DO
            INC (x)
         END ;
         tryTakingRook (b, colour, r, x, 0) ;
         x := -1 ;
         WHILE tryMovingRook (b, colour, r, x, 0) DO
            DEC (x)
         END ;
         tryTakingRook (b, colour, r, x, 0) ;
         y := 1 ;
         WHILE tryMovingRook (b, colour, r, 0, y) DO
            INC (y)
         END ;
         tryTakingRook (b, colour, r, 0, y) ;
         y := -1 ;
         WHILE tryMovingRook (b, colour, r, 0, y) DO
            DEC (y)
         END ;
         tryTakingRook (b, colour, r, 0, y)
      END ;
      INC (r)
   END
END genRookMoves ;


(*
   genPawnMoves -
*)

PROCEDURE genPawnMoves (VAR b: Board; col: Colour) ;
VAR
   pawn, n: CARDINAL8 ;
BEGIN
   n := b.pieces[col].nPawns ;
   pawn := 0 ;
   WHILE pawn < n DO
      IF b.pieces[col].pawn[pawn].xy < UnusedSquare
      THEN
         tryMovingPawn (b, col, pawn, 0, 1) ;
         IF onFirstRow (b, col, pawn) AND secondRowEmpty (b, col, pawn)
         THEN
            tryMovingPawn (b, col, pawn, 0, 2)
         END ;
         tryMovingPawn (b, col, pawn, -1, 1) ;
         tryMovingPawn (b, col, pawn, 1, 1)
      END ;
      INC (pawn)
   END
END genPawnMoves ;


(*
   genQueenMoves -
*)

PROCEDURE genQueenMoves (VAR b: Board; colour: Colour) ;
VAR
   nQueens,
   q       : CARDINAL ;
   x, y    : INTEGER8 ;
BEGIN
   nQueens := b.pieces[colour].nQueens ;
   q := 0 ;
   WHILE q < nQueens DO
      IF b.pieces[colour].queen[q].xy < UnusedSquare
      THEN
         (* diagonal 1, 1.  *)
         x := 1 ;
         WHILE tryMovingQueen (b, colour, q, x, x) DO
            INC (x)
         END ;
         tryTakingQueen (b, colour, q, x, x) ;

         (* diagonal -1, -1.  *)
         x := -1 ;
         WHILE tryMovingQueen (b, colour, q, x, x) DO
            DEC (x)
         END ;
         tryTakingQueen (b, colour, q, x, x) ;

         (* diagonal 1, -1.  *)
         x := 1 ;
         y := -1 ;
         WHILE tryMovingQueen (b, colour, q, x, y) DO
            INC (x) ;
            DEC (y)
         END ;
         tryTakingQueen (b, colour, q, x, y) ;

         (* diagonal -1, 1.  *)
         x := -1 ;
         y := 1 ;
         WHILE tryMovingQueen (b, colour, q, x, y) DO
            DEC (x) ;
            INC (y)
         END ;
         tryTakingQueen (b, colour, q, x, y) ;

         (* axis 0, 1.  *)
         y := 1 ;
         WHILE tryMovingQueen (b, colour, q, 0, y) DO
            INC (y)
         END ;
         tryTakingQueen (b, colour, q, 0, y) ;

         (* axis 0, -1.  *)
         y := -1 ;
         WHILE tryMovingQueen (b, colour, q, 0, y) DO
            DEC (y)
         END ;
         tryTakingQueen (b, colour, q, 0, y) ;

         (* axis 1, 0.  *)
         x := 1 ;
         WHILE tryMovingQueen (b, colour, q, x, 0) DO
            INC (x)
         END ;
         tryTakingQueen (b, colour, q, x, 0) ;

         (* axis -1, 0.  *)
         x := -1 ;
         WHILE tryMovingQueen (b, colour, q, x, 0) DO
            DEC (x)
         END ;
         tryTakingQueen (b, colour, q, x, 0)
      END ;
      INC (q)
   END
END genQueenMoves ;


(*
   genBishopMoves -
*)

PROCEDURE genBishopMoves (VAR b: Board; colour: Colour) ;
BEGIN
   genWBishopMoves (b, colour) ;
   genBBishopMoves (b, colour)
END genBishopMoves ;


(*
   genWBishopMoves -
*)

PROCEDURE genWBishopMoves (VAR b: Board; colour: Colour) ;
VAR
   nBishops,
   i       : CARDINAL ;
   x, y    : INTEGER8 ;
BEGIN
   nBishops := b.pieces[colour].nWBishops ;
   i := 0 ;
   WHILE i < nBishops DO
      IF b.pieces[colour].wbishop[i].xy < UnusedSquare
      THEN
         (* x = 1, y = 1.  *)
         x := 1 ;
         WHILE tryMovingWBishop (b, colour, i, x, x) DO
            INC (x)
         END ;
         tryTakingWBishop (b, colour, i, x, x) ;
         (* x = -1, y = -1.  *)
         x := -1 ;
         WHILE tryMovingWBishop (b, colour, i, x, x) DO
            DEC (x)
         END ;
         tryTakingWBishop (b, colour, i, x, x) ;
         (* x = 1, y = -1.  *)
         x := 1 ;
         y := -1 ;
         WHILE tryMovingWBishop (b, colour, i, x, y) DO
            INC (x) ;
            DEC (y)
         END ;
         tryTakingWBishop (b, colour, i, x, y) ;
         (* x = -1, y = 1.  *)
         x := -1 ;
         y := 1 ;
         WHILE tryMovingWBishop (b, colour, i, x, y) DO
            DEC (x) ;
            INC (y)
         END ;
         tryTakingWBishop (b, colour, i, x, y)
      END ;
      INC (i)
   END
END genWBishopMoves ;


(*
   tryMovingWBishop -
*)

PROCEDURE tryMovingWBishop (VAR b: Board; colour: Colour; i: CARDINAL; x, y: INTEGER8) : BOOLEAN ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[colour].wbishop[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF isEmpty (b, to)
      THEN
         tryMovePiece (b, colour, i + MIN (WBishopRange), xy, to) ;
         RETURN TRUE
      END
   END ;
   RETURN FALSE
END tryMovingWBishop ;


(*
   tryMovingBBishop -
*)

PROCEDURE tryMovingBBishop (VAR b: Board; colour: Colour; i: CARDINAL; x, y: INTEGER8) : BOOLEAN ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[colour].bbishop[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF isEmpty (b, to)
      THEN
         tryMovePiece (b, colour, i + MIN (BBishopRange), xy, to) ;
         RETURN TRUE
      END
   END ;
   RETURN FALSE
END tryMovingBBishop ;


(*
   tryTakingWBishop -
*)

PROCEDURE tryTakingWBishop (VAR b: Board; c: Colour; i: CARDINAL; x, y: INTEGER8) ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[c].wbishop[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF (NOT isEmpty (b, to)) AND (getColour (b, to) # c)
      THEN
         tryMoveTakePiece (b, c, i + MIN (WBishopRange), xy, to)
      END
   END
END tryTakingWBishop ;


(*
   tryTakingBBishop -
*)

PROCEDURE tryTakingBBishop (VAR b: Board; c: Colour; i: CARDINAL; x, y: INTEGER8) ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[c].bbishop[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF (NOT isEmpty (b, to)) AND (getColour (b, to) # c)
      THEN
         tryMoveTakePiece (b, c, i + MIN (BBishopRange), xy, to)
      END
   END
END tryTakingBBishop ;


(*
   genBBishopMoves -
*)

PROCEDURE genBBishopMoves (VAR b: Board; colour: Colour) ;
VAR
   nBishops,
   i       : CARDINAL ;
   x, y    : INTEGER8 ;
BEGIN
   nBishops := b.pieces[colour].nBBishops ;
   i := 0 ;
   WHILE i < nBishops DO
      IF b.pieces[colour].bbishop[i].xy < UnusedSquare
      THEN
         (* x = 1, y = 1.  *)
         x := 1 ;
         WHILE tryMovingBBishop (b, colour, i, x, x) DO
            INC (x)
         END ;
         tryTakingBBishop (b, colour, i, x, x) ;
         (* x = -1, y = -1.  *)
         x := -1 ;
         WHILE tryMovingBBishop (b, colour, i, x, x) DO
            DEC (x)
         END ;
         tryTakingBBishop (b, colour, i, x, x) ;
         (* x = 1, y = -1.  *)
         x := 1 ;
         y := -1 ;
         WHILE tryMovingBBishop (b, colour, i, x, y) DO
            INC (x) ;
            DEC (y)
         END ;
         tryTakingBBishop (b, colour, i, x, y) ;
         (* x = -1, y = 1.  *)
         x := -1 ;
         y := 1 ;
         WHILE tryMovingBBishop (b, colour, i, x, y) DO
            DEC (x) ;
            INC (y)
         END ;
         tryTakingBBishop (b, colour, i, x, y)
      END ;
      INC (i)
   END
END genBBishopMoves ;


(*
   genKnightMoves -
*)

PROCEDURE genKnightMoves (VAR b: Board; colour: Colour) ;
VAR
   nKnights,
   i       : CARDINAL ;
   x, y    : INTEGER8 ;
BEGIN
   nKnights := b.pieces[colour].nKnights ;
   i := 0 ;
   WHILE i < nKnights DO
      IF b.pieces[colour].knight[i].xy < UnusedSquare
      THEN
         tryMovingKnight (b, colour, i, 1, 2) ;
         tryMovingKnight (b, colour, i, 2, 1) ;
         tryMovingKnight (b, colour, i, -1, 2) ;
         tryMovingKnight (b, colour, i, -2, 1) ;
         tryMovingKnight (b, colour, i, 1, -2) ;
         tryMovingKnight (b, colour, i, 2, -1) ;
         tryMovingKnight (b, colour, i, -1, -2) ;
         tryMovingKnight (b, colour, i, -2, -1)
      END ;
      INC (i)
   END
END genKnightMoves ;


(*
   tryMovingKnight -
*)

PROCEDURE tryMovingKnight (VAR b: Board; colour: Colour; i: CARDINAL; x, y: INTEGER8) ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[colour].knight[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF isEmpty (b, to)
      THEN
         tryMovePiece (b, colour, i + MIN (KnightRange), xy, to)
      ELSIF getColour (b, to) # colour
      THEN
         tryMoveTakePiece (b, colour, i + MIN (KnightRange), xy, to)
      END
   END
END tryMovingKnight ;


(*
   onFirstRow -
*)

PROCEDURE onFirstRow (VAR b: Board; col: Colour; i: CARDINAL) : BOOLEAN ;
VAR
   y: CARDINAL8 ;
BEGIN
   y := b.pieces[col].pawn[i].xy DIV BoardSize ;
   RETURN ( ((y=1) AND (col=white)) OR
            ((y=6) AND (col=black)) )
END onFirstRow ;


(*
   secondRowEmpty -
*)

PROCEDURE secondRowEmpty (VAR b: Board; col: Colour; i: CARDINAL) : BOOLEAN ;
BEGIN
   IF col=white
   THEN
      RETURN isEmpty (b, b.pieces[col].pawn[i].xy+BoardSize)
   ELSE
      RETURN isEmpty (b, b.pieces[col].pawn[i].xy-BoardSize)
   END
END secondRowEmpty ;


(*
   isEmpty -
*)

PROCEDURE isEmpty (VAR b: Board; p: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN NOT isUsed (b, p)
END isEmpty ;


(*
   isUsed -
*)

PROCEDURE isUsed (VAR b: Board; p: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN b.square[p].used
END isUsed ;


(*
   isLastRow -
*)

PROCEDURE isLastRow (to: CARDINAL8; col: Colour) : BOOLEAN ;
BEGIN
   RETURN ((col=white) AND ((to DIV BoardSize) = 7)) OR
          ((col=black) AND ((to DIV BoardSize) = 0))
END isLastRow ;


(*
   addPiece -
*)

PROCEDURE addPiece (p: PieceIndex; col: Colour; sq: CARDINAL8; last: BOOLEAN) ;
VAR
   i: InstructionAdd ;
BEGIN
   i.last := last ;
   i.opcode := add ;
   i.colour := col ;
   i.pieceno := p ;
   i.xy := sq ;
   put (i)
END addPiece ;


(*
   opposite -
*)

PROCEDURE opposite (c: Colour) : Colour ;
BEGIN
   IF c=white
   THEN
      RETURN black
   ELSE
      RETURN white
   END
END opposite ;


(*
   buildCons -
*)

PROCEDURE buildCons ;
BEGIN
   (* printf ("buildCons:  takePtr = %d, movePtr = %d\n", takePtr, movePtr) ;  *)
   Assert (moves[movePtr].kind = nop) ;
   moves[movePtr].cons := heapPtr ;
   moves[movePtr].decons := 0
END buildCons ;


(*
   buildDecons -
*)

PROCEDURE buildDecons ;
BEGIN
   moves[movePtr].decons := heapPtr
END buildDecons ;


(*
   orderMove -
*)

PROCEDURE orderMove ;
VAR
   temp: MoveDescriptor ;
BEGIN
   IF (moves[movePtr].kind = tak) AND (takePtr < movePtr)
   THEN
      temp := moves[takePtr] ;
      moves[takePtr] := moves[movePtr] ;
      moves[movePtr] := temp ;
      WHILE (takePtr < movePtr) AND (moves[takePtr].kind = tak) DO
         INC (takePtr)
      END
   END
END orderMove ;


(*
   executeFlags -
*)

PROCEDURE executeFlags (VAR b: Board; inst: InstructionFlags) ;
BEGIN
   IF inst.bccqs
   THEN
      INCL (b.cflags, bqs)
   ELSE
      EXCL (b.cflags, bqs)
   END ;
   IF inst.bccks
   THEN
      INCL (b.cflags, bks)
   ELSE
      EXCL (b.cflags, bks)
   END ;
   IF inst.wccqs
   THEN
      INCL (b.cflags, wqs)
   ELSE
      EXCL (b.cflags, wqs)
   END ;
   IF inst.wccks
   THEN
      INCL (b.cflags, wks)
   ELSE
      EXCL (b.cflags, wks)
   END ;
   IF inst.setNoTake
   THEN
      b.noTake := inst.noTake
   END
END executeFlags ;


(*
   blankSquare -
*)

PROCEDURE blankSquare (VAR b: Board; xy: INTEGER8) ;
BEGIN
   b.square[xy].pix := 0 ;
   b.square[xy].used := FALSE ;
   b.square[xy].colour := black ;
END blankSquare ;


(*
   setSquare -
*)

PROCEDURE setSquare (VAR b: Board; colour: Colour; pix: CARDINAL8; xy: CARDINAL8) ;
BEGIN
   b.square[xy].pix := pix ;
   b.square[xy].used := TRUE ;
   b.square[xy].colour := colour
END setSquare ;


(*
   executeAdd -
*)

PROCEDURE executeAdd (VAR b: Board; inst: InstructionAdd) ;
VAR
   pix: CARDINAL8 ;
BEGIN
   pix := inst.pieceno ;
   CASE pix OF

   MIN(PawnRange)..MAX(PawnRange)      :  executeAddPawn (b, inst.colour, pix) |
   MIN(ShadowRange)..MAX(ShadowRange)  :  executeAddShadow (b, inst.colour, pix) |
   MIN(KnightRange)..MAX(KnightRange)  :  executeAddKnight (b, inst.colour, pix) |
   MIN(WBishopRange)..MAX(WBishopRange):  executeAddWBishop (b, inst.colour, pix) |
   MIN(BBishopRange)..MAX(BBishopRange):  executeAddBBishop (b, inst.colour, pix) |
   MIN(RookRange)..MAX(RookRange)      :  executeAddRook (b, inst.colour, pix) |
   MIN(QueenRange)..MAX(QueenRange)    :  executeAddQueen (b, inst.colour, pix) |
   MIN(KingRange)..MAX(KingRange)      :  HALT

   END ;
   setPieceXY (b, inst.colour, pix, inst.xy) ;
   setSquare (b, inst.colour, pix, inst.xy)
END executeAdd ;


(*
   max8 -
*)

PROCEDURE max8 (a, b: CARDINAL8) : CARDINAL8 ;
BEGIN
   IF a > b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END max8 ;


(*
   executeAddPawn -
*)

PROCEDURE executeAddPawn (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
BEGIN
   b.pieces[colour].nPawns := max8 (pix+1, b.pieces[colour].nPawns) ;
   scoreInc (b, colour, pieceValue[pawn])
END executeAddPawn ;


(*
   executeSubPawn -
*)

PROCEDURE executeSubPawn (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
BEGIN
   (* b.pieces[colour].pawn[pix - MIN (PawnRange)].xy := UnusedSquare ; *)
   IF b.pieces[colour].nPawns = pix+1
   THEN
      WHILE (pix > 0) AND (b.pieces[colour].pawn[pix - MIN (PawnRange)].xy = UnusedSquare) DO
         DEC (pix) ;
         DEC (b.pieces[colour].nPawns)
      END
   END ;
   scoreDec (b, colour, pieceValue[pawn])
END executeSubPawn ;


(*
   executeAddShadow -
*)

PROCEDURE executeAddShadow (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
BEGIN
   b.pieces[colour].nShadowPawn := 1
END executeAddShadow ;


(*
   executeSubShadow -
*)

PROCEDURE executeSubShadow (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
BEGIN
   (* blankSquare (b, b.pieces[colour].shadowPawn.xy) ; *)
   b.pieces[colour].shadowPawn.xy := UnusedSquare ;
   b.pieces[colour].nShadowPawn := 0 ;
END executeSubShadow ;


(*
   executeAddKnight -
*)

PROCEDURE executeAddKnight (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   kix: CARDINAL8 ;
BEGIN
   kix := pix - MIN (KnightRange) ;
   b.pieces[colour].nKnights := max8 (kix+1, b.pieces[colour].nKnights) ;
   scoreInc (b, colour, pieceValue[knight])
END executeAddKnight ;


(*
   executeSubKnight -
*)

PROCEDURE executeSubKnight (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   kix: CARDINAL8 ;
BEGIN
   kix := pix - MIN (KnightRange) ;
   (* blankSquare (b, b.pieces[colour].knight[kix].xy) ;
      b.pieces[colour].knight[kix].xy := UnusedSquare ; *)
   IF b.pieces[colour].nKnights = kix+1
   THEN
      WHILE (kix > 0) AND (b.pieces[colour].knight[kix].xy = UnusedSquare) DO
         DEC (kix) ;
         DEC (b.pieces[colour].nKnights)
      END
   END ;
   scoreDec (b, colour, pieceValue[knight])
END executeSubKnight ;


(*
   executeAddWBishop -
*)

PROCEDURE executeAddWBishop (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   bix: CARDINAL8 ;
BEGIN
   bix := pix - MIN (WBishopRange) ;
   b.pieces[colour].nWBishops := max8 (bix+1, b.pieces[colour].nWBishops) ;
   scoreInc (b, colour, pieceValue[bishop])
END executeAddWBishop ;


(*
   executeSubWBishop -
*)

PROCEDURE executeSubWBishop (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   bix: CARDINAL8 ;
BEGIN
   bix := pix - MIN (WBishopRange) ;
   (* blankSquare (b, b.pieces[colour].wbishop[bix].xy) ;
      b.pieces[colour].wbishop[bix].xy := UnusedSquare ;  *)
   IF b.pieces[colour].nWBishops = bix+1
   THEN
      WHILE (bix > 0) AND (b.pieces[colour].wbishop[bix].xy = UnusedSquare) DO
         DEC (bix) ;
         DEC (b.pieces[colour].nWBishops)
      END
   END ;
   scoreDec (b, colour, pieceValue[bishop])
END executeSubWBishop ;


(*
   executeAddBBishop -
*)

PROCEDURE executeAddBBishop (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   bix: CARDINAL8 ;
BEGIN
   bix := pix - MIN (BBishopRange) ;
   b.pieces[colour].nBBishops := max8 (bix+1, b.pieces[colour].nBBishops) ;
   scoreInc (b, colour, pieceValue[bishop])
END executeAddBBishop ;


(*
   executeSubBBishop -
*)

PROCEDURE executeSubBBishop (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   bix: CARDINAL8 ;
BEGIN
   bix := pix - MIN (BBishopRange) ;
   (* blankSquare (b, b.pieces[colour].bbishop[bix].xy) ;
      b.pieces[colour].bbishop[bix].xy := UnusedSquare ;  *)
   IF b.pieces[colour].nBBishops = bix+1
   THEN
      WHILE (bix > 0) AND (b.pieces[colour].bbishop[bix].xy = UnusedSquare) DO
         DEC (bix) ;
         DEC (b.pieces[colour].nBBishops)
      END
   END ;
   scoreDec (b, colour, pieceValue[bishop])
END executeSubBBishop ;


(*
   executeAddRook -
*)

PROCEDURE executeAddRook (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   rix: CARDINAL8 ;
BEGIN
   rix := pix - MIN (RookRange) ;
   b.pieces[colour].nRooks := max8 (rix+1, b.pieces[colour].nRooks) ;
   scoreInc (b, colour, pieceValue[rook])
END executeAddRook ;


(*
   executeSubRook -
*)

PROCEDURE executeSubRook (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   rix: CARDINAL8 ;
BEGIN
   rix := pix - MIN (RookRange) ;
   (* blankSquare (b, b.pieces[colour].rook[rix].xy) ;
      b.pieces[colour].rook[rix].xy := UnusedSquare ;  *)
   IF b.pieces[colour].nRooks = rix+1
   THEN
      WHILE (rix > 0) AND (b.pieces[colour].rook[rix].xy = UnusedSquare) DO
         DEC (rix) ;
         DEC (b.pieces[colour].nRooks)
      END
   END ;
   scoreDec (b, colour, pieceValue[rook])
END executeSubRook ;


(*
   executeAddQueen -
*)

PROCEDURE executeAddQueen (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   qix: CARDINAL8 ;
BEGIN
   qix := pix - MIN (QueenRange) ;
   b.pieces[colour].nQueens := max8 (qix+1, b.pieces[colour].nQueens) ;
   scoreInc (b, colour, pieceValue[queen])
END executeAddQueen ;


(*
   executeSubQueen -
*)

PROCEDURE executeSubQueen (VAR b: Board; colour: Colour; pix: CARDINAL8) ;
VAR
   qix: CARDINAL8 ;
BEGIN
   qix := pix - MIN (QueenRange) ;
   (* blankSquare (b, b.pieces[colour].queen[qix].xy) ;
      b.pieces[colour].queen[qix].xy := UnusedSquare ;  *)
   IF b.pieces[colour].nQueens = qix+1
   THEN
      WHILE (qix > 0) AND (b.pieces[colour].queen[qix].xy = UnusedSquare) DO
         DEC (qix) ;
         DEC (b.pieces[colour].nQueens)
      END
   END ;
   scoreDec (b, colour, pieceValue[queen])
END executeSubQueen ;


(*
   executeSub -
*)

PROCEDURE executeSub (VAR b: Board; inst: InstructionSub) ;
VAR
   xy    : CARDINAL8 ;
   pix   : CARDINAL8 ;
   colour: Colour ;
BEGIN
   xy := inst.xy ;
   colour := b.square[xy].colour ;
   pix := b.square[xy].pix ;
   setPieceXY (b, colour, pix, UnusedSquare) ;  (* blank this value.  *)
   blankSquare (b, xy) ;   (* blank this square as well.  *)
   CASE pix OF

   MIN(PawnRange)..MAX(PawnRange)      :  executeSubPawn (b, colour, pix) |
   MIN(ShadowRange)..MAX(ShadowRange)  :  b.pieces[colour].nShadowPawn := 0 |
   MIN(KnightRange)..MAX(KnightRange)  :  executeSubKnight (b, colour, pix) |
   MIN(WBishopRange)..MAX(WBishopRange):  executeSubWBishop (b, colour, pix) |
   MIN(BBishopRange)..MAX(BBishopRange):  executeSubBBishop (b, colour, pix) |
   MIN(RookRange)..MAX(RookRange)      :  executeSubRook (b, colour, pix) |
   MIN(QueenRange)..MAX(QueenRange)    :  executeSubQueen (b, colour, pix) |
   MIN(KingRange)..MAX(KingRange)      :  HALT

   END
END executeSub ;


(*
   executeMove -
*)

PROCEDURE executeMove (VAR b: Board; inst: InstructionMove) ;
VAR
   xy : INTEGER8 ;
   pix: PieceIndex ;
BEGIN
   (* xy := getPieceXY (b, inst.colour, inst.pieceno) ; *)
   pix := inst.pieceno ;
   xy := getPieceXY (b, inst.colour, pix) ;
   Assert (xy < MaxSquares) ;
   setPieceXY (b, inst.colour, pix, inst.xy) ;
   b.square[inst.xy] := b.square[xy] ;
   blankSquare (b, xy)
END executeMove ;


(*
   decode -
*)

PROCEDURE decode (VAR b: Board; current: WORD16) ;
VAR
   inst: Instruction ;
BEGIN
   inst := current ;
   CASE inst.opcode OF

   move :  executeMove (b, current) |
   add  :  executeAdd (b, current) |
   sub  :  executeSub (b, current) |
   flags:  executeFlags (b, current)

   END
END decode ;


(*
   execute -
*)

PROCEDURE execute (VAR b: Board; pc: HeapRange) ;
VAR
   current: Instruction ;
BEGIN
   REPEAT
      current := heap[pc] ;
      decode (b, current) ;
      INC (pc)
   UNTIL current.last
END execute ;


(*
   removeShadowPawn -
*)

PROCEDURE removeShadowPawn (VAR b: Board; c: Colour) ;
BEGIN
   IF b.pieces[c].nShadowPawn > 0
   THEN
      blankSquare (b, b.pieces[c].shadowPawn.xy) ;
      b.pieces[c].nShadowPawn := 0 ;
      b.pieces[c].shadowPawn.xy := UnusedSquare
   END
END removeShadowPawn ;


(*
   executeForward - execute a move constructor and advance the full board count.
*)

PROCEDURE executeForward (VAR b: Board; c: Colour; mix: MoveRange) ;
BEGIN
   execute (b, moves[mix].cons) ;
   IF c = black
   THEN
      INC (b.fullMoveNo)
   END
END executeForward ;


(*
   executeBackward - execute a move deconstructor and decrement the full board count.
*)

PROCEDURE executeBackward (VAR b: Board; c: Colour; mix: MoveRange) ;
BEGIN
   execute (b, moves[mix].decons) ;
   IF c = black
   THEN
      DEC (b.fullMoveNo)
   END
END executeBackward ;


(*
   tryCons -
*)

PROCEDURE tryCons (VAR b: Board; col: Colour) ;
VAR
   pre, post: Board ;
BEGIN
   (* printf ("begin tryCons:  takePtr = %d, movePtr = %d\n", takePtr, movePtr) ; *)
   IF Stress
   THEN
      pre := b ;
      IF DebugGeneration
      THEN
         IF debugPrettyMove (b, moves[movePtr].cons) = 0
         THEN
         END ;
         IF DebugMicroCode
         THEN
            dumpMicrocode (b, moves[movePtr].cons, unusedPlyHeap)
         END
      END ;
      pushBoard (stringBoard (b))
   END ;
   execute (b, moves[movePtr].cons) ;
   IF Stress
   THEN
      post := b
   END ;
   IF inCheck (b, col)
   THEN
      IF DebugGeneration
      THEN
         printf ("no good, our king is in check\n")
      END ;
      execute (b, moves[movePtr].decons) ;
      (* illegal move, back out.  *)
      moves[movePtr].kind := nop ;
      IF Stress
      THEN
         assertPopBoard (stringBoard (b), pre, post, b, moves[movePtr].cons, moves[movePtr].decons)
      END
   ELSE
      execute (b, moves[movePtr].decons) ;
      IF Stress
      THEN
         assertPopBoard (stringBoard (b), pre, post, b, moves[movePtr].cons, moves[movePtr].decons)
      END ;
      orderMove ;
      INC (movePtr) ;
      moves[movePtr].kind := nop
   END
   (* printf ("end tryCons:  takePtr = %d, movePtr = %d\n", takePtr, movePtr) ; *)
END tryCons ;


(*
   setTak - assigns the current move as a taking move.
*)

PROCEDURE setTak ;
BEGIN
   moves[movePtr].kind := tak
END setTak ;


(*
   setMov - assigns the current move as a normal move.
*)

PROCEDURE setMov ;
BEGIN
   moves[movePtr].kind := mov
END setMov ;


(*
   checkShadowSub -
*)

PROCEDURE checkShadowSub (VAR b: Board; col: Colour; last: BOOLEAN) ;
BEGIN
   IF b.pieces[col].nShadowPawn > 1
   THEN
      subPiece (b, b.pieces[col].shadowPawn.xy, last)
   END
END checkShadowSub ;


(*
   checkShadowAdd -
*)

PROCEDURE checkShadowAdd (VAR b: Board; col: Colour; last: BOOLEAN) ;
VAR
   xy: CARDINAL8 ;
BEGIN
   xy := b.pieces[col].shadowPawn.xy ;
   IF b.pieces[col].nShadowPawn > 1
   THEN
      addPiece (b.square[xy].pix, col, xy, last)
   END
END checkShadowAdd ;


(*
   tryMovePiece -
*)

PROCEDURE tryMovePiece (VAR b: Board; col: Colour; movpiece: INTEGER8; movefrom, moveto: INTEGER8) ;
VAR
   cflags: cFlags ;
BEGIN
   buildCons ;
   checkShadowSub (b, col, FALSE) ;
   cflags := calcCastling (b.cflags, movefrom, moveto) ;
   IF isPawnIndex (movpiece)
   THEN
      encodeFlags (cflags, TRUE, 0, FALSE)
   ELSE
      encodeFlags (cflags, TRUE, b.noTake + 1, FALSE)
   END ;
   movePiece (b, col, movpiece, moveto, TRUE) ;
   buildDecons ;
   checkShadowAdd (b, col, FALSE) ;
   encodeFlags (b.cflags, TRUE, b.noTake, FALSE) ;
   movePiece (b, col, movpiece, movefrom, TRUE) ;
   setMov ;
   tryCons (b, col)
END tryMovePiece ;


(*
   tryMoveAddPiece - only ever called from the pawn movement.
*)

PROCEDURE tryMoveAddPiece (VAR b: Board; col: Colour;
                           movpiece: INTEGER8; movefrom, moveto: INTEGER8;
                           addpiece: PieceIndex; addto: INTEGER8) ;
VAR
   cflags: cFlags ;
BEGIN
   buildCons ;
   cflags := calcCastling (b.cflags, movefrom, moveto) ;
   checkShadowSub (b, col, FALSE) ;
   encodeFlags (cflags, TRUE, 0, FALSE) ;
   movePiece (b, col, movpiece, moveto, FALSE) ;
   addPiece (addpiece, col, addto, TRUE) ;
   buildDecons ;
   (* as this procedure is only ever called from the pawn movement
      there is no need for the conditional around the encodeFlags.
      IF isPawnIndex (movpiece) OR (b.cflags # cflags)
      THEN

      END ;
    *)
   checkShadowAdd (b, col, FALSE) ;
   encodeFlags (b.cflags, TRUE, b.noTake, FALSE) ;
   subPiece (b, addto, FALSE) ;
   movePiece (b, col, movpiece, movefrom, TRUE) ;
   setMov ;
   tryCons (b, col)
END tryMoveAddPiece ;


(*
   scoreInc - increments the score.  A higher score value for black is negative.
*)

PROCEDURE scoreInc (VAR b: Board; c: Colour; value: INTEGER) ;
BEGIN
   IF c = white
   THEN
      INC (b.score, value)
   ELSE
      DEC (b.score, value)
   END
END scoreInc ;


(*
   scoreDec - decrements the score.  A higher score value for black is positive.
*)

PROCEDURE scoreDec (VAR b: Board; c: Colour; value: INTEGER) ;
BEGIN
   IF c = white
   THEN
      DEC (b.score, value)
   ELSE
      INC (b.score, value)
   END
END scoreDec ;


(*
   getPieceT -
*)

PROCEDURE getPieceT (VAR b: Board; xy: CARDINAL8) : PieceT ;
VAR
   c: Colour ;
BEGIN
   c := b.square[xy].colour ;
   CASE b.square[xy].pix OF

   MIN(PawnRange)..MAX(PawnRange)      :  RETURN pawn |
   MIN(ShadowRange)..MAX(ShadowRange)  :  RETURN shadow |
   MIN(KnightRange)..MAX(KnightRange)  :  RETURN knight |
   MIN(WBishopRange)..MAX(WBishopRange):  RETURN bishop |
   MIN(BBishopRange)..MAX(BBishopRange):  RETURN bishop |
   MIN(RookRange)..MAX(RookRange)      :  RETURN rook |
   MIN(QueenRange)..MAX(QueenRange)    :  RETURN queen |
   MIN(KingRange)..MAX(KingRange)      :  RETURN king

   END
END getPieceT ;


(*
   calcCastling -
*)

PROCEDURE calcCastling (cflags: cFlags; from, to: INTEGER8) : cFlags ;
BEGIN
   cflags := excludeCastlingRook (cflags, to) ;
   IF from = WhiteKing
   THEN
      cflags := excludeCastling (cflags, white)
   ELSIF from = BlackKing
   THEN
      cflags := excludeCastling (cflags, black)
   ELSE
      cflags := excludeCastlingRook (cflags, from)
   END ;
   RETURN cflags
END calcCastling ;


(*
   tryMoveTakePiece - most pieces move onto the square and take a piece at the same time.
*)

PROCEDURE tryMoveTakePiece (VAR b: Board; col: Colour;
                            movpiece: INTEGER8; movefrom, moveto: INTEGER8) ;
VAR
   cflags: cFlags ;
   taken : PieceIndex ;
BEGIN
   taken := b.square[moveto].pix ;
   buildCons ;
   checkShadowSub (b, col, FALSE) ;
   cflags := calcCastling (b.cflags, movefrom, moveto) ;
   encodeFlags (cflags, TRUE, 0, FALSE) ;
   subPiece (b, moveto, FALSE) ;
   movePiece (b, col, movpiece, moveto, TRUE) ;
   buildDecons ;
   checkShadowAdd (b, col, FALSE) ;
   encodeFlags (b.cflags, TRUE, b.noTake, FALSE) ;
   movePiece (b, col, movpiece, movefrom, FALSE) ;
   addPiece (taken, opposite (col), moveto, TRUE) ;
   setTak ;
   tryCons (b, col)
END tryMoveTakePiece ;


(*
   trySubAddPiece - used for pawn move and promotion (without taking).
*)

PROCEDURE trySubAddPiece (VAR b: Board; col: Colour;
                          sub1: INTEGER8; pix: PieceIndex; addto: INTEGER8) ;
VAR
   cflags  : cFlags ;
   taken1  : PieceIndex ;
   col1    : Colour ;
BEGIN
   taken1 := b.square[sub1].pix ;
   col1 := b.square[sub1].colour ;
   buildCons ;
   checkShadowSub (b, col, FALSE) ;
   cflags := calcCastling (b.cflags, sub1, addto) ;
   encodeFlags (cflags, TRUE, 0, FALSE) ;
   subPiece (b, sub1, FALSE) ;  (* remove the first piece.  *)
   addPiece (pix, col, addto, TRUE) ;  (* add the new piece.  *)
   buildDecons ;
   checkShadowAdd (b, col, FALSE) ;
   encodeFlags (b.cflags, TRUE, b.noTake, FALSE) ;
   subPiece (b, addto, FALSE) ;
   addPiece (taken1, col1, sub1, TRUE) ;
   setMov ;
   tryCons (b, col)
END trySubAddPiece ;


(*
   trySubSubAddPiece - only used when a pawn reaches the end and takes a piece as it promotes.
*)

PROCEDURE trySubSubAddPiece (VAR b: Board; col: Colour;
                             sub1, sub2: INTEGER8; pix: PieceIndex; addto: INTEGER8) ;
VAR
   cflags  : cFlags ;
   taken1,
   taken2  : PieceIndex ;
   col1,
   col2    : Colour ;
BEGIN
   taken1 := b.square[sub1].pix ;
   taken2 := b.square[sub2].pix ;
   col1 := b.square[sub1].colour ;
   col2 := b.square[sub2].colour ;
   buildCons ;
   checkShadowSub (b, col, FALSE) ;
   cflags := calcCastling (b.cflags, sub1, addto) ;
   encodeFlags (cflags, TRUE, 0, FALSE) ;
   subPiece (b, sub1, FALSE) ;  (* remove the first piece.  *)
   subPiece (b, sub2, FALSE) ;  (* remove the second piece.  *)
   addPiece (pix, col, addto, TRUE) ;  (* add the new piece.  *)
   buildDecons ;
   checkShadowAdd (b, col, FALSE) ;
   (* as we are moving a pawn we need to remember the 50 move count.  *)
   encodeFlags (b.cflags, TRUE, b.noTake, FALSE) ;
   subPiece (b, addto, FALSE) ;
   addPiece (taken2, col2, sub2, FALSE) ;
   addPiece (taken1, col1, sub1, TRUE) ;
   setTak ;
   tryCons (b, col)
END trySubSubAddPiece ;


(*
   trySubSubMovePiece - only used for taking en-passent pawns.
*)

PROCEDURE trySubSubMovePiece (VAR b: Board; col: Colour;
                              sub1, sub2: INTEGER8; p: INTEGER8; from, to: INTEGER8) ;
VAR
   cflags  : cFlags ;
   taken1,
   taken2  : PieceIndex ;
   col1,
   col2    : Colour ;
BEGIN
   taken1 := b.square[sub1].pix ;
   taken2 := b.square[sub2].pix ;
   col1 := b.square[sub1].colour ;
   col2 := b.square[sub2].colour ;
   buildCons ;
   checkShadowSub (b, col, FALSE) ;
   cflags := calcCastling (b.cflags, from, to) ;   (* this is unnecessary for shadow pawns.  *)
   encodeFlags (cflags, TRUE, 0, FALSE) ;
   subPiece (b, sub1, FALSE) ;  (* remove the first piece.  *)
   subPiece (b, sub2, FALSE) ;  (* remove the second piece.  *)
   movePiece (b, col, p, to, TRUE) ;  (* add the new piece.  *)
   buildDecons ;
   checkShadowAdd (b, col, FALSE) ;
   (* as we are moving a pawn we need to remember the 50 move count.  *)
   encodeFlags (b.cflags, TRUE, b.noTake, FALSE) ;
   movePiece (b, col, p, from, FALSE) ;
   addPiece (taken2, col2, sub2, FALSE) ;
   addPiece (taken1, col1, sub1, TRUE) ;
   setTak ;
   tryCons (b, col)
END trySubSubMovePiece ;


(*
   tryMovingPawn -
*)

PROCEDURE tryMovingPawn (VAR b: Board; col: Colour; pawn: CARDINAL8; dx, dy: INTEGER) ;
VAR
   es,
   nx, ny,
   to, sq,
   en    : INTEGER8 ;
   opp   : Colour ;
BEGIN
   sq := b.pieces[col].pawn[pawn].xy ;
   IF col=black
   THEN
      dy := -dy ;
      opp := white
   ELSE
      opp := black
   END ;
   IF inRange (sq, dx, dy, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF dx=0
      THEN
         (* movement only *)
         IF isEmpty (b, to)
         THEN
            IF isLastRow (to, col)
            THEN
               (* promotion *)
               trySubAddPiece (b, col, sq, b.pieces[col].nKnights + MIN (KnightRange), to) ;
               IF ODD (sq)
               THEN
                  trySubAddPiece (b, col, sq, b.pieces[col].nBBishops + MIN (BBishopRange), to)
               ELSE
                  trySubAddPiece (b, col, sq, b.pieces[col].nWBishops + MIN (WBishopRange), to)
               END ;
               trySubAddPiece (b, col, sq, b.pieces[col].nRooks + MIN (RookRange), to) ;
               trySubAddPiece (b, col, sq, b.pieces[col].nQueens + MIN (QueenRange), to)
            ELSE
               IF TRUE AND ((dy = 2) OR (dy = -2))
               THEN
                  IF dy = 2
                  THEN
                     es := to - BoardSize
                  ELSE
                     es := to + BoardSize
                  END ;
                  tryMoveAddPiece (b, col, pawn, sq, to, MIN (ShadowRange), es) ;
                  printf ("after adding enpassent\n");
                  displayBoard (b)
               ELSE
                  tryMovePiece (b, col, pawn, sq, to)
               END
            END
         END
      ELSE
         (* try taking a shadow pawn.  col is the colour taking the pawn.  *)
         IF isShadow (b, opp, to)
         THEN
            (* encode an enpassent move.  *)
            IF col=white
            THEN
               (* delete shadow pawn, delete real pawn, move col pawn.   *)
               trySubSubMovePiece (b, col, to, to-BoardSize, pawn, sq, to)
            ELSE
               (* delete shadow pawn, delete real pawn, move col pawn.   *)
               trySubSubMovePiece (b, col, to, to+BoardSize, pawn, sq, to)
            END ;
         ELSIF isOpponentOccupied (b, col, to)
         THEN
            IF isLastRow (to, col)
            THEN
               (* take a piece and also promote *)
               trySubSubAddPiece (b, col, sq, to, b.pieces[col].nKnights + MIN (KnightRange), to) ;
               IF ODD (sq)
               THEN
                  trySubSubAddPiece (b, col, sq, to, b.pieces[col].nBBishops + MIN (BBishopRange), to)
               ELSE
                  trySubSubAddPiece (b, col, sq, to, b.pieces[col].nWBishops + MIN (WBishopRange), to)
               END ;
               trySubSubAddPiece (b, col, sq, to, b.pieces[col].nRooks + MIN (RookRange), to) ;
               trySubSubAddPiece (b, col, sq, to, b.pieces[col].nQueens + MIN (QueenRange), to)
            ELSE
               tryMoveTakePiece (b, col, pawn, sq, to)
            END
         END
      END
   END
END tryMovingPawn ;


(*
   movePiece -
*)

PROCEDURE movePiece (VAR b: Board; col: Colour; pieceno: CARDINAL8;
                     to: CARDINAL8; last: BOOLEAN) ;
VAR
   i: InstructionMove ;
BEGIN
   i.last := last ;
   i.opcode := move ;
   i.pieceno := pieceno ;
   i.colour := col ;
   i.xy := to ;
   put (i)
END movePiece ;


(*
   encodeFlags - encode the castling flags into an instruction.
*)

PROCEDURE encodeFlags (cflags: cFlags; setNoTake: BOOLEAN; noTake: NoTakeT; last: BOOLEAN) ;
VAR
   i: InstructionFlags ;
BEGIN
   i.opcode := flags ;
   i.last := last ;
   i.bccqs := bqs IN cflags ;
   i.bccks := bks IN cflags ;
   i.wccqs := wqs IN cflags ;
   i.wccks := wks IN cflags ;
   i.setNoTake := setNoTake ;
   i.noTake := noTake ;
   put (i)
END encodeFlags ;


(*
   subPiece -
*)

PROCEDURE subPiece (VAR b: Board; sq: CARDINAL8; last: BOOLEAN) ;
VAR
   i: InstructionSub ;
BEGIN
   IF isKing (b, b.square[sq].colour, sq)
   THEN
      printf ("assert will fail as the move is about to take the king!\n");
      displayBoard (b)
   END ;
   Assert (NOT isKing (b, b.square[sq].colour, sq)) ;
   i.last := last ;
   i.opcode := sub ;
   i.xy := sq ;
   put (i)
END subPiece ;


(*
   addTake - encode a take.  The board, b, pieceNo, of colour, col, is moving to square, to.
*)

PROCEDURE addTake (VAR b: Board; col: Colour; pieceNo: CARDINAL8; to: CARDINAL8; last: BOOLEAN) ;
BEGIN
   subPiece (b, to, FALSE) ;
   movePiece (b, col, pieceNo, to, last)
END addTake ;


(*
   inCheck - returns TRUE if the king of colour, c, is in check.
*)

PROCEDURE inCheck (VAR b: Board; c: Colour) : BOOLEAN ;
BEGIN
   RETURN squareInCheck (b, opposite (c), b.pieces[c].king.xy)
END inCheck ;


(*
   availableMoves -
*)

PROCEDURE availableMoves (VAR b: Board; c: Colour) : CARDINAL ;
VAR
   fd   : FrameDescriptor ;
   count: CARDINAL ;
BEGIN
   fd := saveFrame () ;
   genMoves (b, c) ;
   count := movePtr - fd.movePtr ;
   restoreFrame (fd) ;
   RETURN count
END availableMoves ;


(*
   isCheckMate -
*)

PROCEDURE isCheckMate (VAR b: Board; c: Colour) : BOOLEAN ;
BEGIN
   RETURN inCheck (b, c) AND (availableMoves (b, c) = 0)
END isCheckMate ;


(*
   isStaleMate -
*)

PROCEDURE isStaleMate (VAR b: Board; c: Colour) : BOOLEAN ;
BEGIN
   RETURN (NOT inCheck (b, c)) AND (availableMoves (b, c) = 0)
END isStaleMate ;


(*
   isAxisCheck - return TRUE if square, pos, is in check from a piece of colour c along the
                 vector [dx, dy].
*)

PROCEDURE isAxisCheck (VAR b: Board; c: Colour; pos: CARDINAL8; dx, dy: INTEGER8) : BOOLEAN ;
VAR
   count: INTEGER8 ;
   xy   : CARDINAL8 ;
   x, y : INTEGER8 ;
BEGIN
   IF inRange (pos, dx, dy, x, y)
   THEN
      (* test whether the king controls the square.  *)
      xy := XY (x, y) ;
      IF b.pieces[c].king.xy = xy
      THEN
         RETURN TRUE
      END ;
      count := 1 ;
      WHILE (count < BoardSize) DO
         IF inRange (pos, dx * count, dy * count, x, y)
         THEN
            xy := XY (x, y) ;
            IF isUsed (b, xy)
            THEN
               IF isRook (b, c, xy) OR isQueen (b, c, xy)
               THEN
                  RETURN TRUE
               END ;
               RETURN FALSE
            END
         ELSE
            RETURN FALSE
         END ;
         INC (count)
      END
   END ;
   RETURN FALSE
END isAxisCheck ;


(*
   isDiagCheck - return TRUE if square, pos, is in check from a piece of colour c along the
                 vector [dx, dy].
*)

PROCEDURE isDiagCheck (VAR b: Board; c: Colour; pos: CARDINAL8; dx, dy: INTEGER8) : BOOLEAN ;
VAR
   p       : CARDINAL8 ;
   count   : INTEGER8 ;
   xy      : CARDINAL8 ;
   x, y    : INTEGER8 ;
BEGIN
   IF inRange (pos, dx, dy, x, y)
   THEN
      (* test whether the king controls the square.  *)
      xy := XY (x, y) ;
      IF b.pieces[c].king.xy = xy
      THEN
         RETURN TRUE
      END ;
      (* check for a pawn providing that the vector is in the correct direction.  *)
      IF ((c = white) AND (dy < 0)) OR
         ((c = black) AND (dy > 0))
      THEN
         (* test whether a pawn controls the square.  *)
         p := 0 ;
         WHILE p < b.pieces[c].nPawns DO
            IF b.pieces[c].pawn[p].xy = xy
            THEN
               RETURN TRUE
            END ;
            INC (p)
         END
      END ;
      (* and now test for bishops and queens.  *)
      count := 1 ;
      WHILE (count < BoardSize) DO
         IF inRange (pos, dx * count, dy * count, x, y)
         THEN
            xy := XY (x, y) ;
            IF isUsed (b, xy)
            THEN
               IF isBishop (b, c, xy) OR isQueen (b, c, xy)
               THEN
                  RETURN TRUE
               END ;
               RETURN FALSE
            END
         ELSE
            RETURN FALSE
         END ;
         INC (count)
      END
   END ;
   RETURN FALSE
END isDiagCheck ;


(*
   isKnightCheck -
*)

PROCEDURE isKnightCheck  (VAR b: Board; c: Colour; pos: CARDINAL8; dx, dy: INTEGER8) : BOOLEAN ;
VAR
   n   : CARDINAL8 ;
   xy  : CARDINAL8 ;
   x, y: INTEGER8 ;
BEGIN
   IF inRange (pos, dx, dy, x, y)
   THEN
      xy := XY (x, y) ;
      (* test whether a knight controls the square.  *)
      n := 0 ;
      WHILE n < b.pieces[c].nKnights DO
         IF b.pieces[c].knight[n].xy = xy
         THEN
            RETURN TRUE
         END ;
         INC (n)
      END
   END ;
   RETURN FALSE
END isKnightCheck ;


(*
   squareInCheck - is square, pos, in check from colour, c, on board, b.
*)

PROCEDURE squareInCheck (VAR b: Board; c: Colour; pos: CARDINAL8) : BOOLEAN ;
BEGIN
   IF isAxisCheck (b, c, pos, 1, 0) OR isAxisCheck (b, c, pos, 0, 1) OR
      isAxisCheck (b, c, pos, -1, 0) OR isAxisCheck (b, c, pos, 0, -1)
   THEN
      RETURN TRUE
   ELSIF isDiagCheck (b, c, pos, 1, 1) OR isDiagCheck (b, c, pos, -1, 1) OR
         isDiagCheck (b, c, pos, 1, -1) OR isDiagCheck (b, c, pos, -1, -1)
   THEN
      RETURN TRUE
   ELSIF isKnightCheck (b, c, pos, 1, 2) OR isKnightCheck (b, c, pos, -1, 2) OR
         isKnightCheck (b, c, pos, 1, -2) OR isKnightCheck (b, c, pos, -1, -2) OR
         isKnightCheck (b, c, pos, 2, 1) OR isKnightCheck (b, c, pos, -2, 1) OR
         isKnightCheck (b, c, pos, 2, -1) OR isKnightCheck (b, c, pos, -2, -1)
   THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END squareInCheck ;


(*
   get -
*)

PROCEDURE get (i: CARDINAL) : WORD16 ;
BEGIN
   RETURN heap[i]
END get ;


(*
   isPawn -
*)

PROCEDURE isPawn (VAR b: Board; c: Colour; xy: CARDINAL8) : BOOLEAN ;
VAR
   p: CARDINAL8 ;
BEGIN
   p := 0 ;
   WHILE p < b.pieces[c].nPawns DO
      IF b.pieces[c].pawn[p].xy = xy
      THEN
         RETURN TRUE
      END ;
      INC (p)
   END ;
   RETURN FALSE
END isPawn ;


(*
   isShadow -
*)

PROCEDURE isShadow (VAR b: Board; col: Colour; xy: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN (b.pieces[col].nShadowPawn > 0) AND
          (b.pieces[col].shadowPawn.xy = xy)
END isShadow ;


(*
   isKnight -
*)

PROCEDURE isKnight (VAR b: Board; c: Colour; xy: CARDINAL8) : BOOLEAN ;
VAR
   i: CARDINAL8 ;
BEGIN
   i := 0 ;
   WHILE i < b.pieces[c].nKnights DO
      IF b.pieces[c].knight[i].xy = xy
      THEN
         RETURN TRUE
      END ;
      INC (i)
   END ;
   RETURN FALSE
END isKnight ;


(*
   isBishop -
*)

PROCEDURE isBishop (VAR b: Board; c: Colour; xy: CARDINAL8) : BOOLEAN ;
VAR
   i: CARDINAL8 ;
BEGIN
   i := 0 ;
   WHILE i < b.pieces[c].nWBishops DO
      IF b.pieces[c].wbishop[i].xy = xy
      THEN
         RETURN TRUE
      END ;
      INC (i)
   END ;
   i := 0 ;
   WHILE i < b.pieces[c].nBBishops DO
      IF b.pieces[c].bbishop[i].xy = xy
      THEN
         RETURN TRUE
      END ;
      INC (i)
   END ;
   RETURN FALSE
END isBishop ;


(*
   isRook -
*)

PROCEDURE isRook (VAR b: Board; c: Colour; xy: CARDINAL8) : BOOLEAN ;
VAR
   r: CARDINAL8 ;
BEGIN
   r := 0 ;
   WHILE r < b.pieces[c].nRooks DO
      IF b.pieces[c].rook[r].xy = xy
      THEN
         RETURN TRUE
      END ;
      INC (r)
   END ;
   RETURN FALSE
END isRook ;


(*
   isQueen -
*)

PROCEDURE isQueen (VAR b: Board; c: Colour; xy: CARDINAL8) : BOOLEAN ;
VAR
   q: CARDINAL8 ;
BEGIN
   q := 0 ;
   WHILE q < b.pieces[c].nQueens DO
      IF b.pieces[c].queen[q].xy = xy
      THEN
         RETURN TRUE
      END ;
      INC (q)
   END ;
   RETURN FALSE
END isQueen ;


(*
   isKing -
*)

PROCEDURE isKing (VAR b: Board; c: Colour; xy: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN b.pieces[c].king.xy = xy
END isKing ;


(*
   isPawnIndex -
*)

PROCEDURE isPawnIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN (pix >= MIN (PawnRange)) AND (pix <= MAX (PawnRange))
END isPawnIndex ;


(*
   isShadowIndex -
*)

PROCEDURE isShadowIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN pix = MIN (ShadowRange)
END isShadowIndex ;


(*
   isKnightIndex -
*)

PROCEDURE isKnightIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN (pix >= MIN (KnightRange)) AND (pix <= MAX (KnightRange))
END isKnightIndex ;


(*
   isWBishop -
*)

PROCEDURE isWBishopIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN (pix >= MIN (WBishopRange)) AND (pix <= MAX (WBishopRange))
END isWBishopIndex ;


(*
   isBBishop -
*)

PROCEDURE isBBishopIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN (pix >= MIN (BBishopRange)) AND (pix <= MAX (BBishopRange))
END isBBishopIndex ;


(*
   isBishopIndex -
*)

PROCEDURE isBishopIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN isWBishopIndex (pix) OR isBBishopIndex (pix)
END isBishopIndex ;


(*
   isRookIndex -
*)

PROCEDURE isRookIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN (pix >= MIN (RookRange)) AND (pix <= MAX (RookRange))
END isRookIndex ;


(*
   isQueenIndex -
*)

PROCEDURE isQueenIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN (pix >= MIN (QueenRange)) AND (pix <= MAX (QueenRange))
END isQueenIndex ;


(*
   isKingIndex -
*)

PROCEDURE isKingIndex (pix: CARDINAL8) : BOOLEAN ;
BEGIN
   RETURN pix = MIN (KingRange)
END isKingIndex ;


(*
   getPieceChar -
*)

PROCEDURE getPieceChar (c: Colour; pix: CARDINAL8) : CHAR ;
BEGIN
   IF isPawnIndex (pix)
   THEN
      RETURN colourChar (c, 'p')
   ELSIF isShadowIndex (pix)
   THEN
      RETURN colourChar (c, 'e')
   ELSIF isKnightIndex (pix)
   THEN
      RETURN colourChar (c, 'n')
   ELSIF isBishopIndex (pix)
   THEN
      RETURN colourChar (c, 'b')
   ELSIF isRookIndex (pix)
   THEN
      RETURN colourChar (c, 'r')
   ELSIF isQueenIndex (pix)
   THEN
      RETURN colourChar (c, 'q')
   ELSE
      Assert (isKingIndex (pix)) ;
      RETURN colourChar (c, 'k')
   END
END getPieceChar ;


(*
   getPieceXY - return the square on which, pieceno, resides.
*)

PROCEDURE getPieceXY (VAR b: Board; c: Colour; pieceno: PieceIndex) : INTEGER8 ;
BEGIN
   WITH b.pieces[c] DO
      CASE pieceno OF

      MIN(PawnRange)..MAX(PawnRange)      :  RETURN pawn[pieceno].xy |
      MIN(ShadowRange)..MAX(ShadowRange)  :  RETURN shadowPawn.xy |
      MIN(KnightRange)..MAX(KnightRange)  :  RETURN knight[pieceno - MIN (KnightRange)].xy |
      MIN(WBishopRange)..MAX(WBishopRange):  RETURN wbishop[pieceno - MIN (WBishopRange)].xy |
      MIN(BBishopRange)..MAX(BBishopRange):  RETURN bbishop[pieceno - MIN (BBishopRange)].xy |
      MIN(RookRange)..MAX(RookRange)      :  RETURN rook[pieceno - MIN (RookRange)].xy |
      MIN(QueenRange)..MAX(QueenRange)    :  RETURN queen[pieceno - MIN (QueenRange)].xy |
      MIN(KingRange)..MAX(KingRange)      :  RETURN king.xy |

      ELSE
         HALT
      END
   END
END getPieceXY ;


(*
   setPieceXY - return the square on which, pieceno, resides.
*)

PROCEDURE setPieceXY (VAR b: Board; c: Colour; pieceno: PieceIndex; xy: INTEGER8) ;
BEGIN
   WITH b.pieces[c] DO
      CASE pieceno OF

      MIN(PawnRange)..MAX(PawnRange)      :  pawn[pieceno].xy := xy |
      MIN(ShadowRange)..MAX(ShadowRange)  :  shadowPawn.xy := xy |
      MIN(KnightRange)..MAX(KnightRange)  :  knight[pieceno - MIN (KnightRange)].xy := xy |
      MIN(WBishopRange)..MAX(WBishopRange):  wbishop[pieceno - MIN (WBishopRange)].xy := xy |
      MIN(BBishopRange)..MAX(BBishopRange):  bbishop[pieceno - MIN (BBishopRange)].xy := xy |
      MIN(RookRange)..MAX(RookRange)      :  rook[pieceno - MIN (RookRange)].xy := xy |
      MIN(QueenRange)..MAX(QueenRange)    :  queen[pieceno - MIN (QueenRange)].xy := xy |
      MIN(KingRange)..MAX(KingRange)      :  king.xy := xy

      ELSE
         printf ("invalid pieceno value %d\n", VAL (INTEGER, pieceno)) ;
         HALT
      END
   END
END setPieceXY ;


(*
   prettyCoord -
*)

PROCEDURE prettyCoord (s: CARDINAL8) ;
VAR
   x, y: CARDINAL ;
BEGIN
   x := s MOD BoardSize ;
   y := s DIV BoardSize ;
   printf ("%c%d", CHR (ORD ("A")+x), y+1)
END prettyCoord ;


(*
   getMoveSrcXY -
*)

PROCEDURE getMoveSrcXY (VAR b: Board; m: InstructionMove) : CARDINAL8 ;
VAR
   pix: PieceIndex ;
BEGIN
   Assert (m.opcode = move) ;
   pix := m.pieceno ;
   RETURN getPieceXY (b, m.colour, pix)
END getMoveSrcXY ;


(*
   getKindChar -
*)

PROCEDURE getKindChar (VAR b: Board; c: Colour; k: PieceT) : CHAR ;
BEGIN
   CASE k OF

   none  :  RETURN '.' |
   pawn  :  RETURN colourChar (c, 'p') |
   shadow:  RETURN colourChar (c, 'e') |
   knight:  RETURN colourChar (c, 'n') |
   bishop:  RETURN colourChar (c, 'b') |
   rook  :  RETURN colourChar (c, 'r') |
   queen :  RETURN colourChar (c, 'q') |
   king  :  RETURN colourChar (c, 'k')

   END
END getKindChar ;


(*
   dumpMove -
*)

PROCEDURE dumpMove (VAR b: Board; m: InstructionMove) ;
VAR
   pix: PieceIndex ;
BEGIN
   Assert (m.opcode = move) ;
   pix := m.pieceno ;
   printf ("mov %c", getPieceChar (m.colour, pix)) ;
   IF getPieceXY (b, m.colour, pix) = m.xy
   THEN
      printf ("..")
   ELSE
      prettyCoord (getPieceXY (b, m.colour, pix))
   END ;
   printf ("-") ;
   prettyCoord (m.xy)
END dumpMove ;


(*
   dumpAdd -
*)

PROCEDURE dumpAdd (VAR b: Board; a: InstructionAdd) ;
BEGIN
   Assert (a.opcode = add) ;
   printf ("add %c", getPieceChar (a.colour, a.pieceno)) ;
   prettyCoord (a.xy)
END dumpAdd ;


(*
   dumpSub -
*)

PROCEDURE dumpSub (s: InstructionSub) ;
BEGIN
   Assert (s.opcode = sub) ;
   printf ("sub ") ;
   prettyCoord (s.xy)
END dumpSub ;


(*
   prettyFlag -
*)

PROCEDURE prettyFlag (b: BOOLEAN) ;
BEGIN
   IF b
   THEN
      printf ("1")
   ELSE
      printf ("0")
   END
END prettyFlag ;


(*
   dumpFlags -
*)

PROCEDURE dumpFlags (f: InstructionFlags) ;
BEGIN
   Assert (f.opcode = flags) ;
   printf ("flags ") ;
   prettyFlag (f.bccqs) ;
   prettyFlag (f.bccks) ;
   prettyFlag (f.wccqs) ;
   prettyFlag (f.wccks) ;
   IF f.setNoTake
   THEN
      printf (", %d", f.noTake)
   END
END dumpFlags ;


(*
   dumpMoves -
*)

PROCEDURE dumpMoves (VAR b: Board; m: MoveRange; ply: plyRange) ;
BEGIN
   printf ("%d   cons   ", m) ;
   dumpMicrocode (b, moves[m].cons, ply) ;
   printf ("\n") ;
   printf ("    [decons ", m) ;
   dumpMicrocode (b, moves[m].decons, ply) ;
   printf ("]\n") ;
END dumpMoves ;


(*
   dumpMicrocode -
*)

PROCEDURE dumpMicrocode (VAR b: Board; i: HeapRange; ply: plyRange) ;
VAR
   head    : Instruction ;
   n, j    : HeapRange ;
   consMove: ARRAY [0.. MAX (HeapRange)] OF WORD16 ;
BEGIN
   n := getMove (i, consMove) ;
   j := 0 ;
   WHILE j < n DO
      head := consMove[j] ;
      CASE head.opcode OF

      move :  dumpMove (b, consMove[j]) |
      add  :  dumpAdd (b, consMove[j]) |
      sub  :  dumpSub (consMove[j]) |
      flags:  dumpFlags (consMove[j])

      END ;
      INC (j) ;
      IF j < n
      THEN
         printf (" ; ")
      END
   END
END dumpMicrocode ;


(*
   contains -
*)

PROCEDURE contains (VAR b: ARRAY OF WORD16; code: HeapOpcode) : BOOLEAN ;
VAR
   head: Instruction ;
   i, n: CARDINAL ;
BEGIN
   i := 0 ;
   n := HIGH (b) ;
   WHILE i <= n DO
      head := b[i] ;
      IF head.opcode = code
      THEN
         RETURN TRUE
      END ;
      IF head.last
      THEN
         RETURN FALSE
      END ;
      INC (i)
   END ;
   RETURN FALSE
END contains ;


(*
   getAddPieceNo - returns the pieceno specified by the first add instruction.
*)

PROCEDURE getAddPieceNo (VAR b: Board; VAR cons: ARRAY OF WORD16; VAR c: Colour) : PieceIndex ;
VAR
   inst: InstructionAdd ;
   i, n: CARDINAL ;
BEGIN
   n := HIGH (cons) ;
   i := 0 ;
   WHILE i <= n DO
      inst := cons[i] ;
      IF inst.opcode = add
      THEN
         c := inst.colour ;
         RETURN inst.pieceno
      ELSIF inst.last
      THEN
         HALT
      END ;
      INC (i)
   END ;
   HALT
END getAddPieceNo ;


(*
   getAddChar -
*)

PROCEDURE getAddChar (VAR b: Board; VAR cons: ARRAY OF BYTE) : CHAR ;
VAR
   c  : Colour ;
   pix: PieceIndex ;
BEGIN
   pix := getAddPieceNo (b, cons, c) ;
   RETURN getPieceChar (c, pix)
END getAddChar ;


(*
   getSrcDestColour -
*)

PROCEDURE getSrcDestColour (VAR b: Board; VAR cons: ARRAY OF WORD16;
                            VAR src, dest: CARDINAL8; VAR c: Colour) ;
VAR
   a      : InstructionAdd ;
   m      : InstructionMove ;
   s      : InstructionSub ;
   inst   : Instruction ;
   i, n   : CARDINAL ;
   seenAdd,
   seenSub: BOOLEAN ;
   pix    : PieceIndex ;
BEGIN
   seenAdd := FALSE ;
   seenSub := FALSE ;
   n := HIGH (cons) ;
   i := 0 ;
   WHILE i <= n DO
      inst := cons[i] ;
      IF debugging
      THEN
         printf ("%d   %02x \n", i, inst)
      END ;
      IF inst.opcode = move
      THEN
         m := cons[i] ;
         c := m.colour ;
         pix := m.pieceno ;
         src := getPieceXY (b, c, pix) ;
         dest := m.xy ;
         IF debugging
         THEN
            printf ("seen move, pieceno = %d, c = %d, dest = %d, src = %d\n", pix, c, dest, src)
         END ;
         (* we have now seen all three values so we return.  *)
         RETURN
      ELSIF inst.opcode = sub
      THEN
         IF debugging
         THEN
            printf ("skipping sub\n")
         END ;
         IF inst.last
         THEN
            i := n
         END
      ELSIF inst.opcode = add
      THEN
         IF debugging
         THEN
            printf ("skipping add\n")
         END ;
         IF inst.last
         THEN
            i := n
         END
      END ;
      INC (i)
   END ;
   (* no move seen - obtain values from add and sub.  *)
   i := 0 ;
   WHILE (i <= n) AND ((NOT seenAdd) OR (NOT seenSub)) DO
      inst := cons[i] ;
      IF inst.opcode = add
      THEN
         a := cons[i] ;
         IF NOT seenAdd
         THEN
            seenAdd := TRUE ;
            c := a.colour ;
            dest := a.xy ;
            IF debugging
            THEN
               printf ("seen add, dest = %d\n", dest)
            END
         END
      ELSIF inst.opcode = sub
      THEN
         s := cons[i] ;
         IF NOT seenSub
         THEN
            seenSub := TRUE ;
            src := s.xy ;
            IF debugging
            THEN
               printf ("seen sub, src = %d\n", src)
            END
         END
      END ;
      INC (i)
   END ;
   Assert (seenAdd AND seenSub)
END getSrcDestColour ;


(*
   getXYChar -
*)

PROCEDURE getXYChar (VAR b: Board; c: Colour; xy: CARDINAL8) : CHAR ;
BEGIN
   IF isPawn (b, c, xy)
   THEN
      RETURN colourChar (c, 'p')
   ELSIF isShadow (b, c, xy)
   THEN
      RETURN colourChar (c, 'e')
   ELSIF isKnight (b, c, xy)
   THEN
      RETURN colourChar (c, 'n')
   ELSIF isBishop (b, c, xy)
   THEN
      RETURN colourChar (c, 'b')
   ELSIF isRook (b, c, xy)
   THEN
      RETURN colourChar (c, 'r')
   ELSIF isQueen (b, c, xy)
   THEN
      RETURN colourChar (c, 'q')
   ELSIF isKing (b, c, xy)
   THEN
      RETURN colourChar (c, 'k')
   ELSE
      HALT
   END
END getXYChar ;


(*
   prettyMove -
*)

PROCEDURE prettyMove (VAR b: Board; i: HeapRange) : HeapRange ;
BEGIN
   RETURN doPrettyMove (b, i, TRUE)
END prettyMove ;


(*
   debugPrettyMove -
*)

PROCEDURE debugPrettyMove (VAR b: Board; i: HeapRange) : HeapRange ;
BEGIN
   RETURN doPrettyMove (b, i, FALSE)
END debugPrettyMove ;


(*
   doPrettyMove -
*)

PROCEDURE doPrettyMove (VAR b: Board; i: HeapRange; calcStatus: BOOLEAN) : HeapRange ;
VAR
   c        : Colour ;
   src, dest: CARDINAL8 ;
   n        : HeapRange ;
   consMove : ARRAY [0.. MAX (HeapRange)] OF WORD16 ;
   copy     : Board ;
   fd       : FrameDescriptor ;
BEGIN
   n := getMove (i, consMove) ;
   getSrcDestColour (b, consMove, src, dest, c) ;
   IF contains (consMove, sub)
   THEN
      IF contains (consMove, add)
      THEN
         IF isPawn (b, c, src)
         THEN
            IF (src MOD BoardSize) = (dest MOD BoardSize)
            THEN
               IF isLastRow (dest, c)
               THEN
                  (* pawn promotion.  *)
                  printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
                  printf ("-") ; prettyCoord (dest) ; printf ("%c", getAddChar (b, consMove))
               ELSE
                  (* pawn move.  *)
                  printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
                  printf ("-") ; prettyCoord (dest)
               END
            ELSE
               IF isLastRow (dest, c)
               THEN
                  (* pawn take and promotion.  *)
                  printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
                  printf ("x") ; prettyCoord (dest) ; printf ("%c", getAddChar (b, consMove))
               ELSE
                  (* pawn take.  *)
                  printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
                  printf ("x") ; prettyCoord (dest)
               END
            END
         ELSE
            (* turn involves a take.  *)
            printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
            printf ("x") ; prettyCoord (dest)
         END
      ELSE
         (* turn involves a take.  *)
         printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
         printf ("x") ; prettyCoord (dest)
      END
   ELSE
      IF isKing (b, c, src)
      THEN
         IF src - 3 = dest
         THEN
            (* queen side castle.  *)
            printf ("%c-%c", colourChar (c, 'o'), colourChar (c, 'o'))
         ELSIF src + 2 = dest
         THEN
            (* king side castle.  *)
            printf ("%c-%c-%c", colourChar (c, 'o'), colourChar (c, 'o'), colourChar (c, 'o'))
         ELSE
            (* normal move.  *)
            printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
            printf ("-") ; prettyCoord (dest)
         END
      ELSE
         (* normal move.  *)
         printf ("%c", getXYChar (b, c, src)) ; prettyCoord (src) ;
         printf ("-") ; prettyCoord (dest)
      END
   END ;
   IF calcStatus
   THEN
      (* This code should be re-thought as it might be better to calculate the
         status during the move generation or post move during alpha-beta.
         This section must not be called by the debugging routines inside tryMove
         as we end up in a recursive loop.  *)
      copy := b ;
      execute (copy, i) ;
      IF inCheck (copy, opposite (c))
      THEN
         printf ("+") ;
         IF isCheckMate (copy, opposite (c))
         THEN
            printf ("+")
         END
      ELSIF isStaleMate (copy, opposite (c))
      THEN
         printf ("=")
      END
   END ;
   RETURN n
END doPrettyMove ;


(*
   foreachMoveDo -
*)

PROCEDURE foreachMoveDo (VAR b: Board; i, j: MoveRange; ply: plyRange; proc: DecodeProcedure) ;
BEGIN
   WHILE i < j DO
      proc (b, i, ply) ;
      INC (i)
   END
END foreachMoveDo ;


(*
   getMove -
*)

PROCEDURE getMove (l: HeapRange; VAR b: ARRAY OF WORD16) : HeapRange ;
VAR
   i   : HeapRange ;
   head: Instruction ;
BEGIN
   i := 0 ;
   REPEAT
      head := get (l) ;
      b[i] := head ;
      INC (i) ;
      INC (l)
   UNTIL head.last ;
   RETURN i
END getMove ;


(*
   colourChar -
*)

PROCEDURE colourChar (c: Colour; p: CHAR) : CHAR ;
BEGIN
   IF c = white
   THEN
      RETURN CAP (p)
   ELSE
      RETURN p
   END
END colourChar ;


(*
   displayPiece -
*)

PROCEDURE displayPiece (c: Colour; p: CHAR) ;
BEGIN
   printf ("%c", colourChar (c, p))
END displayPiece ;


(*
   displaySquare -
*)

PROCEDURE displaySquare (VAR b: Board; xy: CARDINAL8) ;
BEGIN
   printf ("%c", charSquare (b, xy))
END displaySquare ;


(*
   charSquare -
*)

PROCEDURE charSquare (VAR b: Board; xy: CARDINAL8) : CHAR ;
VAR
   c: Colour ;
BEGIN
   c := b.square[xy].colour ;
   CASE b.square[xy].pix OF

   MIN(PawnRange)..MAX(PawnRange)      :  RETURN colourChar (c, 'p') |
   MIN(ShadowRange)..MAX(ShadowRange)  :  RETURN colourChar (c, 'e') |
   MIN(KnightRange)..MAX(KnightRange)  :  RETURN colourChar (c, 'n') |
   MIN(WBishopRange)..MAX(WBishopRange):  RETURN colourChar (c, 'b') |
   MIN(BBishopRange)..MAX(BBishopRange):  RETURN colourChar (c, 'b') |
   MIN(RookRange)..MAX(RookRange)      :  RETURN colourChar (c, 'r') |
   MIN(QueenRange)..MAX(QueenRange)    :  RETURN colourChar (c, 'q')

   ELSE
      RETURN colourChar (c, 'k')
   END
END charSquare ;


(*
   updateSquare -
*)

PROCEDURE updateSquare (VAR b: Board; c: Colour; x, y: CARDINAL8; ix: PieceIndex) ;
BEGIN
   WITH b.square[XY (x, y)] DO
      pix := ix ;
      used := TRUE ;
      colour := c
   END
END updateSquare ;


(*
   addEnpassent -
*)

PROCEDURE addEnpassent (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      ix := MIN (ShadowRange) + nShadowPawn ;
      nShadowPawn := 1 ;
      shadowPawn.xy := XY (x, y)
   END ;
   updateSquare (b, c, x, y, ix)
END addEnpassent ;


(*
   addPawn -
*)

PROCEDURE addPawn (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      pawn[nPawns].xy := XY (x, y) ;
      ix := nPawns ;
      INC (nPawns)
   END ;
   updateSquare (b, c, x, y, ix)
END addPawn ;


(*
   addKnight -
*)

PROCEDURE addKnight (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      knight[nKnights].xy := XY (x, y) ;
      ix := MIN (KnightRange) + nKnights ;
      INC (nKnights)
   END ;
   updateSquare (b, c, x, y, ix)
END addKnight ;


(*
   addWBishop -
*)

PROCEDURE addWBishop (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      wbishop[nWBishops].xy := XY (x, y) ;
      ix := MIN (WBishopRange) + nWBishops ;
      INC (nWBishops)
   END ;
   updateSquare (b, c, x, y, ix)
END addWBishop ;


(*
   addBBishop -
*)

PROCEDURE addBBishop (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      bbishop[nBBishops].xy := XY (x, y) ;
      ix := MIN (BBishopRange) + nBBishops ;
      INC (nBBishops)
   END ;
   updateSquare (b, c, x, y, ix)
END addBBishop ;


(*
   addBishop -
*)

PROCEDURE addBishop (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
BEGIN
   IF ODD (XY (x, y))
   THEN
      printf ("%d, %d square %d adding white square bishop\n", x, y, XY (x, y));
      addWBishop (b, c, x, y)
   ELSE
      printf ("%d, %d square %d adding black square bishop\n", x, y, XY (x, y));
      addBBishop (b, c, x, y)
   END
END addBishop ;


(*
   addRook -
*)

PROCEDURE addRook (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      rook[nRooks].xy := XY (x, y) ;
      ix := MIN (RookRange) + nRooks ;
      INC (nRooks)
   END ;
   updateSquare (b, c, x, y, ix)
END addRook ;


(*
   addQueen -
*)

PROCEDURE addQueen (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      queen[nQueens].xy := XY (x, y) ;
      ix := MIN (QueenRange) + nQueens ;
      INC (nQueens)
   END ;
   updateSquare (b, c, x, y, ix)
END addQueen ;


(*
   addKing -
*)

PROCEDURE addKing (VAR b: Board; c: Colour; x, y: CARDINAL8) ;
VAR
   ix: CARDINAL8 ;
BEGIN
   WITH b.pieces[c] DO
      king.xy := XY (x, y) ;
      ix := MIN (KingRange)
   END ;
   updateSquare (b, c, x, y, ix)
END addKing ;


(*
   emptyBoard -
*)

PROCEDURE emptyBoard (VAR b: Board) ;
VAR
   c : Colour ;
   sq: CARDINAL8 ;
BEGIN
   FOR c := white TO black DO
      WITH b.pieces[c] DO
         nPawns := 0 ;
         nKnights := 0 ;
         nWBishops := 0 ;
         nBBishops := 0 ;
         nRooks := 0 ;
         nQueens := 0 ;
         nShadowPawn := 0
      END
   END ;
   FOR sq := 0 TO MaxSquares-1 DO
      b.square[sq].pix := 0 ;
      b.square[sq].used := FALSE ;
      b.square[sq].colour := white
   END ;
   b.noTake := 0 ;
   b.fullMoveNo := 1 ;
   (* and neither side can castle in the future.  *)
   b.cflags := cFlags {}
END emptyBoard ;


(*
   initPieces -
*)

PROCEDURE initPieces (VAR b: Board) ;
VAR
   c    : Colour ;
   i, sq: CARDINAL8 ;
BEGIN
   emptyBoard (b) ;

   (* add rows of pawns for white and black.  *)
   FOR i := 0 TO BoardSize-1 DO
      addPawn (b, white, i, 1) ;
      addPawn (b, black, i, 6)
   END ;

   (* white pieces.  *)
   addKnight (b, white, 1, 0) ;
   addKnight (b, white, 6, 0) ;
   IF BoardSize=8
   THEN
      addBishop (b, white, 2, 0) ;
      addBishop (b, white, 5, 0)
   END ;
   addRook (b, white, 0, 0) ;
   addRook (b, white, 7, 0) ;
   addQueen (b, white, 3, 0) ;
   addKing (b, white, 4, 0) ;

   (* black pieces.  *)
   addKnight (b, black, 1, BoardSize-1) ;
   addKnight (b, black, 6, BoardSize-1) ;
   IF BoardSize=8
   THEN
      addBishop (b, black, 2, BoardSize-1) ;
      addBishop (b, black, 5, BoardSize-1)
   END ;
   addRook (b, black, 0, BoardSize-1) ;
   addRook (b, black, 7, BoardSize-1) ;
   addQueen (b, black, 3, BoardSize-1) ;
   addKing (b, black, 4, BoardSize-1) ;

   (* and both sides can castle in the future.  *)
   b.cflags := cFlags {bks, bqs, wks, wqs}
END initPieces ;


(*
   initBoard -
*)

PROCEDURE initBoard (VAR b: Board) ;
BEGIN
   b.noTake := 0 ;
   initPieces (b)
END initBoard ;


(*
   bitChar -
*)

PROCEDURE bitChar (b: BOOLEAN) : CHAR ;
BEGIN
   IF b
   THEN
      RETURN '1'
   ELSE
      RETURN '0'
   END
END bitChar ;


(*
   stringBoard -
*)

PROCEDURE stringBoard (VAR b: Board) : String ;
VAR
   s   : String ;
   r, c: CARDINAL ;
BEGIN
   s := InitString ('') ;
   FOR r := BoardSize-1 TO 0 BY -1 DO
      FOR c := 0 TO BoardSize-1 DO
         IF isUsed (b, XY (c, r))
         THEN
            s := ConCatChar (s, charSquare (b, XY (c, r)))
         ELSE
            s := ConCatChar (s, ".")
         END
      END
   END ;
   s := ConCatChar (s, " ") ;
   s := ConCat (s, ctos (b.noTake, 0, ' ')) ;
   s := ConCatChar (s, " ") ;
   s := ConCatChar (s, bitChar (bks IN b.cflags)) ;
   s := ConCatChar (s, bitChar (bqs IN b.cflags)) ;
   s := ConCatChar (s, bitChar (wks IN b.cflags)) ;
   s := ConCatChar (s, bitChar (wqs IN b.cflags)) ;
   RETURN s
END stringBoard ;


(*
   displayBoard - display the board, b.
*)

PROCEDURE displayBoard (VAR b: Board) ;
VAR
   r, c: CARDINAL ;
BEGIN
   printf ("     a b c d e f g h\n");
   printf ("   +-----------------+\n");
   FOR r := BoardSize-1 TO 0 BY -1 DO
      printf (" %d | ", r+1);
      FOR c := 0 TO BoardSize-1 DO
         IF isUsed (b, XY (c, r))
         THEN
            displaySquare (b, XY (c, r))
         ELSE
            printf (".")
         END ;
         printf (" ")
      END ;
      printf ("| %d\n", r+1);
   END ;
   printf ("   +-----------------+\n");
   printf ("     a b c d e f g h\n");
END displayBoard ;


PROCEDURE printStatus ;
BEGIN
   printf ("move %d, ", currentBoard.fullMoveNo) ;
   IF turn = white
   THEN
      printf ("white to make a move\n")
   ELSE
      printf ("black to make a move\n")
   END ;
   printf ("%d half moves since a pawn was moved or piece taken.\n", currentBoard.fullMoveNo) ;
   IF currentBoard.cflags = cFlags {}
   THEN
      printf ("neither side can castle\n")
   ELSE
      IF (NOT (wks IN currentBoard.cflags)) AND (NOT (wqs IN currentBoard.cflags))
      THEN
         printf ("white cannot castle, ")
      ELSIF (wks IN currentBoard.cflags) AND (wqs IN currentBoard.cflags)
      THEN
         printf ("white can castle either side, ")
      ELSIF wks IN currentBoard.cflags
      THEN
         printf ("white can only castle on the kings side, ")
      ELSIF wqs IN currentBoard.cflags
      THEN
         printf ("white can only castle on the queens side, ")
      END ;
      IF (NOT (bks IN currentBoard.cflags)) AND (NOT (bqs IN currentBoard.cflags))
      THEN
         printf ("black cannot castle")
      ELSIF (bks IN currentBoard.cflags) AND (bqs IN currentBoard.cflags)
      THEN
         printf ("black can castle either side")
      ELSIF bks IN currentBoard.cflags
      THEN
         printf ("black can only castle on the kings side")
      ELSIF bqs IN currentBoard.cflags
      THEN
         printf ("black can only castle on the queens side")
      END ;
      printf ("\n")
   END
END printStatus ;


(*
   printFlags - display the search flags.
*)

PROCEDURE printFlags ;
VAR
   comma: BOOLEAN ;
BEGIN
   IF searchFlags # SearchFlagSet {}
   THEN
      printf ("search flags: ") ;
      comma := FALSE ;
      IF forceblackwin IN searchFlags
      THEN
         printf (" a win for black can be forced") ;
         comma := TRUE
      END ;
      IF forcewhitewin IN searchFlags
      THEN
         IF comma
         THEN
            printf (", ")
         END ;
         printf ("a win for white can be forced") ;
         comma := TRUE
      END ;
      IF forcedraw IN searchFlags
      THEN
         IF comma
         THEN
            printf (", ")
         END ;
         printf ("a draw can be forced")
      END ;
      printf (".\n")
   END ;
END printFlags ;


(*
   tryMovingRook -
*)

PROCEDURE tryMovingRook (VAR b: Board; c: Colour; i: CARDINAL; x, y: INTEGER8) : BOOLEAN ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[c].rook[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF isEmpty (b, to)
      THEN
         tryMovePiece (b, c, i + MIN (RookRange), xy, to) ;
         RETURN TRUE
      END
   END ;
   RETURN FALSE
END tryMovingRook ;


(*
   tryTakingRook -
*)

PROCEDURE tryTakingRook (VAR b: Board; c: Colour; i: CARDINAL; x, y: INTEGER8) ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[c].rook[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF (NOT isEmpty (b, to)) AND (getColour (b, to) # c)
      THEN
         tryMoveTakePiece (b, c, i + MIN (RookRange), xy, to)
      END
   END
END tryTakingRook ;


(*
   tryMovingQueen -
*)

PROCEDURE tryMovingQueen (VAR b: Board; c: Colour; i: CARDINAL; x, y: INTEGER8) : BOOLEAN ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[c].queen[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF isEmpty (b, to)
      THEN
         tryMovePiece (b, c, i + MIN (QueenRange), xy, to) ;
         RETURN TRUE
      END
   END ;
   RETURN FALSE
END tryMovingQueen ;


(*
   tryTakingQueen -
*)

PROCEDURE tryTakingQueen (VAR b: Board; c: Colour; i: CARDINAL; x, y: INTEGER8) ;
VAR
   nx, ny,
   to, xy: INTEGER8 ;
BEGIN
   xy := b.pieces[c].queen[i].xy ;
   IF inRange (xy, x, y, nx, ny)
   THEN
      to := XY (nx, ny) ;
      IF (NOT isEmpty (b, to)) AND (getColour (b, to) # c)
      THEN
         tryMoveTakePiece (b, c, i + MIN (QueenRange), xy, to)
      END
   END
END tryTakingQueen ;


PROCEDURE setMaxProcessors (processors: CARDINAL) ;
BEGIN
   IF m2config.MULTIPROCESSOR
   THEN
      IF processors = 0
      THEN
         maxProcessors := multiprocessor.maxProcessors ()
      ELSE
         maxProcessors := processors
      END
   ELSE
      maxProcessors := 1
   END ;
   printf ("processors available to chess shell: %d\n", maxProcessors)
END setMaxProcessors ;


(*
   register - associate the, pid, of the child process with the, move.
*)

(*
PROCEDURE registerRunning (pid: INTEGER; move: HeapRange; ply: plyRange) : CARDINAL ;
VAR
   p: CARDINAL ;
BEGIN
   WITH plyPool^[ply] DO
      p := nextProcessors ;
      INC (nextProcessors) ;
      processors[p].pstatus := running ;
      processors[p].childPid := pid ;
      processors[p].move := move
   END ;
   RETURN p
END registerRunning ;
*)


(*
   min -

*)

PROCEDURE min (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a < b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END min ;


(*
   maxValue -

*)

PROCEDURE max (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a > b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END max ;


(*
   rememberBest -
*)

PROCEDURE rememberBest (ply: plyRange; turn: Colour; result: INTEGER; m: MoveRange) : INTEGER ;
BEGIN
   printf ("ply = %d, topPoolid = %d, best = %d, result = %d, mix = %d\n", ply, topPoolId, plyPool^[ply].best, result, m) ;
   IF turn = white
   THEN
      IF ply = topPoolId
      THEN
         IF result >= WhiteWin
         THEN
            INCL (searchFlags, forcewhitewin)
         ELSIF 0 IN BITSET (result)
         THEN
            INCL (searchFlags, forcedraw)
         END
      END ;
      IF result >= plyPool^[ply].best
      THEN
         IF ply = topPoolId
         THEN
            plyPool^[ply].bestMove := m ;
            plyPool^[ply].best := result ;
            printf ("  white found better: best = %d, result = %d, mix = %d\n", plyPool^[ply].best, result, m)
         END ;
         RETURN result
      END
   ELSE
      IF ply = topPoolId
      THEN
         IF result <= BlackWin
         THEN
            INCL (searchFlags, forceblackwin)
         ELSIF 0 IN BITSET (result)
         THEN
            INCL (searchFlags, forcedraw)
         END
      END ;
      IF result <= plyPool^[ply].best
      THEN
         IF ply = topPoolId
         THEN
            plyPool^[ply].bestMove := m ;
            plyPool^[ply].best := result ;
            printf ("  black found better: best = %d, result = %d, mix = %d\n", plyPool^[ply].best, result, m)
         END ;
         RETURN result
      END
   END ;
   RETURN plyPool^[ply].best
END rememberBest ;


(*
   isDraw - returns TRUE if there are not enough pieces
            on the board to force a win.
*)

PROCEDURE isDraw (VAR b: Board) : BOOLEAN ;
VAR
   c: Colour ;
BEGIN
   FOR c := MIN (Colour) TO MAX (Colour) DO
      (* can still win with any of these.  *)
      IF b.pieces[c].nPawns + b.pieces[c].nRooks + b.pieces[c].nQueens > 0
      THEN
         RETURN FALSE
      END ;
      (* can still (technically) win with any of these.  *)
      IF b.pieces[c].nKnights + b.pieces[c].nWBishops + b.pieces[c].nBBishops >= 2
      THEN
         RETURN FALSE
      END
   END ;
   RETURN TRUE
END isDraw ;


(*
   stop -
*)

PROCEDURE stop ;
END stop ;


(*
   stop1 -
*)

PROCEDURE stop1 ;
END stop1 ;


(*
   encodeSearch -
*)

PROCEDURE encodeSearch (result: INTEGER; search: SearchFlag) : INTEGER ;
BEGIN
   CASE search OF

   forcedraw    :  RETURN VAL (INTEGER, VAL (BITSET, result) + BITSET {0}) |
   forcewhitewin:  RETURN VAL (INTEGER, VAL (BITSET, result) + BITSET {1}) |
   forceblackwin:  RETURN VAL (INTEGER, VAL (BITSET, result) + BITSET {2})

   END
END encodeSearch ;


(*
   alphaBetaSingle - A move has been applied to board, b, and now it is colour, c,
                     turn to move.  The score of the board is returned.
                     The single processor version of alphaBeta.
*)

PROCEDURE alphaBetaSingle (VAR b: Board; colour: Colour;
                           pliesLeft: INTEGER; alpha, beta: INTEGER) : INTEGER ;
VAR
   try      : INTEGER ;
   i,
   movetop  : CARDINAL ;
   fd       : FrameDescriptor ;
   pre, post: Board ;
BEGIN
   IF pliesLeft = 0
   THEN
      INC (totalMoveCount) ;
      RETURN b.score
   ELSIF isDraw (b)
   THEN
      (* always choose an earlier draw, otherwise
         it might always chase an elusive draw.  *)
      IF colour = white
      THEN
         try := 0 - pliesLeft
      ELSE
         try := pliesLeft
      END ;
      try := try * ScalePly ;
      RETURN encodeSearch (try, forcedraw)
   ELSE
      fd := saveFrame () ;
      genMoves (b, colour) ;
      movetop := movePtr ;
      IF DebugAlphaBeta
      THEN
         foreachMoveDo (b, fd.movePtr, movetop, unusedPlyHeap, prettyList)
      END ;
      IF colour = white
      THEN
         (* white to move.  *)
         IF fd.movePtr = movetop
         THEN
            INC (totalMoveCount) ;
            (* no move available, work out draw or loss  *)
            IF inCheck (b, white)
            THEN
               (* check mate.  *)
               restoreFrame (fd) ;
               IF DebugAlphaBeta
               THEN
                  printf ("seen check mate loss for white\n");
                  displayBoard (b)
               END ;
               try := BlackWin - pliesLeft ;  (* always choose an earlier win.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forceblackwin)
            ELSE
               restoreFrame (fd) ;
               try := 0 - pliesLeft ;  (* always choose an earlier draw, otherwise
                                          it might always chase an elusive draw.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forcedraw)
            END
         ELSE
            i := fd.movePtr ;
            try := MinScore ;
            WHILE i < movetop DO
               INC (totalMoveCount) ;
               IF DebugAlphaBeta
               THEN
                  printf ("\nexamining ");
                  IF prettyMove (b, moves[i].cons) = 0
                  THEN
                  END ;
                  printf ("\n");
                  displayBoard (b)
               END ;
               IF Stress
               THEN
                  pre := b ;
                  pushBoard (stringBoard (b))
               END ;
               executeForward (b, white, i) ;    (* apply constructor.  *)
               IF Stress
               THEN
                  post := b
               END ;
               try := max (alphaBetaSingle (b, black, pliesLeft - 1, alpha, beta), try) ;
               executeBackward (b, white, i) ;    (* apply deconstructor.  *)
               IF Stress
               THEN
                  assertPopBoard (stringBoard (b), pre, post, b, moves[i].cons, moves[i].decons)
               END ;
               IF try > alpha
               THEN
                  (* found a better move *)
                  alpha := try
               END ;
               IF alpha >= beta
               THEN
                  restoreFrame (fd) ;
                  RETURN try
               END ;
               INC (i)
            END
         END ;
         restoreFrame (fd) ;
         RETURN try
      ELSE
         (* black to move.  *)
         IF movetop = fd.movePtr
         THEN
            INC (totalMoveCount) ;
            (* no move available, work out draw or loss  *)
            IF inCheck (b, black)
            THEN
               (* check mate.  *)
               restoreFrame (fd) ;
               IF DebugAlphaBeta
               THEN
                  printf ("seen check mate loss for black\n");
                  displayBoard (b)
               END ;
               try := WhiteWin + pliesLeft ;  (* always choose an earlier win.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forcewhitewin)
            ELSE
               IF DebugAlphaBeta
               THEN
                  printf ("seen draw for black\n")
               END ;
               restoreFrame (fd) ;
               try := 0 + pliesLeft ;  (* always choose an earlier draw, otherwise
                                          it might always chase an elusive draw.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forcedraw)
            END
         ELSE
            i := fd.movePtr ;
            try := MaxScore ;
            WHILE i < movetop DO
               INC (totalMoveCount) ;
               (* apply move.  *)
               IF Stress
               THEN
                  pre := b ;
                  pushBoard (stringBoard (b))
               END ;
               executeForward (b, black, i) ;    (* apply constructor.  *)
               try := min (alphaBetaSingle (b, white, pliesLeft - 1, alpha, beta), try) ;
               IF Stress
               THEN
                  post := b
               END ;
               (* retract the move.  *)
               executeBackward (b, black, i) ;    (* apply deconstructor.  *)
               IF Stress
               THEN
                  assertPopBoard (stringBoard (b), pre, post, b, moves[i].cons, moves[i].decons)
               END ;
               IF try < beta
               THEN
                  (* found a better move *)
                  beta := try
               END ;
               IF alpha >= beta
               THEN
                  (* no point searching further as white would choose
                     a different previous move *)
                  restoreFrame (fd) ;
                  RETURN try
               END ;
               INC (i)
            END
         END ;
         restoreFrame (fd) ;
         RETURN beta   (* the best score for a move Black has found *)
      END
   END
END alphaBetaSingle ;


(*
   alphaBetaMulti - A move has been applied to board, b, and now it is colour, c,
                    turn to move.  The score of the board is returned.
                    The single processor version of alphaBeta.
*)

PROCEDURE alphaBetaMulti (VAR b: Board; colour: Colour;
                          pliesLeft: INTEGER;
                          alpha, beta: INTEGER;
                          plyId: plyRange) : INTEGER ;
VAR
   try      : INTEGER ;
   i,
   movetop  : CARDINAL ;
   fd       : FrameDescriptor ;
   pre, post: Board ;
BEGIN
   IF pliesLeft = 0
   THEN
      INC (totalMoveCount) ;
      RETURN b.score
   ELSIF isDraw (b)
   THEN
      (* always choose an earlier draw, otherwise
         it might always chase an elusive draw.  *)
      IF colour = white
      THEN
         try := 0 - pliesLeft
      ELSE
         try := pliesLeft
      END ;
      try := try * ScalePly ;
      RETURN encodeSearch (try, forcedraw)
   ELSE
      fd := saveFrame () ;
      genMoves (b, colour) ;
      movetop := movePtr ;
      IF DebugAlphaBeta
      THEN
         foreachMoveDo (b, fd.movePtr, movetop, unusedPlyHeap, prettyList)
      END ;
      IF colour = white
      THEN
         (* white to move.  *)
         IF fd.movePtr = movetop
         THEN
            INC (totalMoveCount) ;
            (* no move available, work out draw or loss  *)
            IF inCheck (b, white)
            THEN
               (* check mate.  *)
               restoreFrame (fd) ;
               IF DebugAlphaBeta
               THEN
                  printf ("seen check mate loss for white\n");
                  displayBoard (b)
               END ;
               try := BlackWin - pliesLeft ;  (* always choose an earlier win.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forceblackwin)
            ELSE
               restoreFrame (fd) ;
               try := 0 - pliesLeft ;  (* always choose an earlier draw, otherwise
                                          it might always chase an elusive draw.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forcedraw)
            END
         ELSE
            i := fd.movePtr ;
            try := MinScore ;
            (*
            IF currentAvailable (plyId) > 1   (*   = moveTop - i  *)
            THEN
               (* we can use our available cores and search in parallel.  *)
               printf ("we could search in parallel during this ply: %d, with %d cores\n", pliesLeft, currentAvailable (plyId));
            END ;
            *)
               WHILE i < movetop DO
                  INC (totalMoveCount) ;
                  IF DebugAlphaBeta
                  THEN
                     printf ("\nexamining ");
                     IF prettyMove (b, moves[i].cons) = 0
                     THEN
                     END ;
                     printf ("\n");
                     displayBoard (b)
                  END ;
                  IF Stress
                  THEN
                     pre := b ;
                     pushBoard (stringBoard (b))
                  END ;
                  executeForward (b, white, i) ;    (* apply constructor.  *)
                  IF Stress
                  THEN
                     post := b
                  END ;
                  try := max (alphaBetaMulti (b, black, pliesLeft - 1, alpha, beta, plyId), try) ;
                  executeBackward (b, white, i) ;    (* apply deconstructor.  *)
                  IF Stress
                  THEN
                     assertPopBoard (stringBoard (b), pre, post, b, moves[i].cons, moves[i].decons)
                  END ;
                  IF try > alpha
                  THEN
                     (* found a better move *)
                     alpha := try
                  END ;
                  IF alpha >= beta
                  THEN
                     restoreFrame (fd) ;
                     RETURN try
                  END ;
                  INC (i)
               END
            (* END *)
         END ;
         restoreFrame (fd) ;
         RETURN try
      ELSE
         (* black to move.  *)
         IF movetop = fd.movePtr
         THEN
            INC (totalMoveCount) ;
            (* no move available, work out draw or loss  *)
            IF inCheck (b, black)
            THEN
               (* check mate.  *)
               restoreFrame (fd) ;
               IF DebugAlphaBeta
               THEN
                  printf ("seen check mate loss for black\n");
                  displayBoard (b)
               END ;
               try := WhiteWin + pliesLeft ;  (* always choose an earlier win.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forcewhitewin)
            ELSE
               IF DebugAlphaBeta
               THEN
                  printf ("seen draw for black\n")
               END ;
               restoreFrame (fd) ;
               try := 0 + pliesLeft ;  (* always choose an earlier draw, otherwise
                                          it might always chase an elusive draw.  *)
               try := try * ScalePly ;
               RETURN encodeSearch (try, forcedraw)
            END
         ELSE
            i := fd.movePtr ;
            try := MaxScore ;
            WHILE i < movetop DO
               INC (totalMoveCount) ;
               (* apply move.  *)
               IF Stress
               THEN
                  pre := b ;
                  pushBoard (stringBoard (b))
               END ;
               executeForward (b, black, i) ;    (* apply constructor.  *)
               try := min (alphaBetaMulti (b, white, pliesLeft - 1, alpha, beta, plyId), try) ;
               IF Stress
               THEN
                  post := b
               END ;
               (* retract the move.  *)
               executeBackward (b, black, i) ;    (* apply deconstructor.  *)
               IF Stress
               THEN
                  assertPopBoard (stringBoard (b), pre, post, b, moves[i].cons, moves[i].decons)
               END ;
               IF try < beta
               THEN
                  (* found a better move *)
                  beta := try
               END ;
               IF alpha >= beta
               THEN
                  (* no point searching further as white would choose
                     a different previous move *)
                  restoreFrame (fd) ;
                  RETURN try
               END ;
               INC (i)
            END
         END ;
         restoreFrame (fd) ;
         RETURN beta   (* the best score for a move Black has found *)
      END
   END
END alphaBetaMulti ;


(*
   currentRunning - plyPoolIndex
*)

PROCEDURE currentRunning (plyPoolIndex: CARDINAL) : CARDINAL ;
VAR
   active: CARDINAL ;
BEGIN
   wait (plyPool^[plyPoolIndex].plyMutex) ;
   active := plyPool^[plyPoolIndex].childrenActive ;
   signal (plyPool^[plyPoolIndex].plyMutex) ;
   RETURN active
END currentRunning ;


(*
   currentAvailable - return the number of free processors.
*)

PROCEDURE currentAvailable (plyPoolIndex: CARDINAL) : CARDINAL ;
VAR
   free: CARDINAL ;
BEGIN
   wait (plyPool^[plyPoolIndex].plyMutex) ;
   IF maxProcessors > plyPool^[plyPoolIndex].childrenActive
   THEN
      free := maxProcessors - plyPool^[plyPoolIndex].childrenActive
   ELSE
      free := 0
   END ;
   signal (plyPool^[plyPoolIndex].plyMutex) ;
   RETURN free
END currentAvailable ;


(*
   exploreBoard -
*)

PROCEDURE exploreBoard (VAR b: Board; colour: Colour;
                        pliesLeft: INTEGER; alpha, beta: INTEGER) : INTEGER ;
BEGIN
   RETURN 0
END exploreBoard ;


(*
   exploreBoardAlphaBetaMulti - wrap up the arguments and call alphaBeta.
                                A move has been applied to board, b, and now it is colour, c,
                                turn to move.  The score of the board is returned.
*)

PROCEDURE exploreBoardAlphaBetaMulti (VAR b: Board; c: Colour; best: INTEGER; ply: plyRange) : INTEGER ;
BEGIN
   RETURN alphaBetaMulti (b, c, maxPlies, best, MaxScore, ply)
END exploreBoardAlphaBetaMulti ;


(*
   exploreBoardAlphaBetaSingle - wrap up the arguments and call alphaBeta.
                                 A move has been applied to board, b, and now it is colour, c,
                                 turn to move.  The score of the board is returned.
                                 This is called if we are searching all the moves on a single processor.
*)

PROCEDURE exploreBoardAlphaBetaSingle (VAR b: Board; c: Colour; best: INTEGER) : INTEGER ;
BEGIN
   RETURN alphaBetaSingle (b, c, maxPlies, best, MaxScore)
END exploreBoardAlphaBetaSingle ;

(*
(*
   redMove -
*)

PROCEDURE writeRed (VAR b: Board; m: MoveRange) ;
VAR
   count: CARDINAL ;
BEGIN
   colors.red ; count := prettyMove (b, moves[m].cons) ; colors.reset
END writeRed ;


(*
   writeYellow -
*)

PROCEDURE writeYellow (VAR b: Board; m: MoveRange; pstatus: status) ;
VAR
   count: CARDINAL ;
BEGIN
   colors.yellow ; count := prettyMove (b, moves[m].cons) ;
   printf (" [") ;
   writeStatus (pstatus) ;
   printf ("]") ;
   colors.reset
END writeYellow ;


(*
   writeGreen -
*)

PROCEDURE writeGreen (VAR b: Board; m: MoveRange; pstatus: status; ply: plyRange) ;
VAR
   count: CARDINAL ;
BEGIN
   colors.green ;
   count := prettyMove (b, moves[m].cons) ;
   printf (" [") ;
   writeStatus (pstatus) ;
   printf (": %d]", plyPool^[ply].processors[m].result) ;
   colors.reset
END writeGreen ;


(*
   writeBlue -
*)

PROCEDURE writeBlue (VAR b: Board; m: MoveRange; pstatus: status; ply: plyRange) ;
VAR
   count: CARDINAL ;
BEGIN
   colors.blue ; count := prettyMove (b, moves[m].cons) ;
   printf (" [") ;
   writeStatus (pstatus) ;
   printf (": %d]", plyPool^[ply].processors[m].result) ;
   colors.reset
END writeBlue ;


(*
   writeMagenta -
*)

PROCEDURE writeMagenta (VAR b: Board; m: MoveRange; pstatus: status; ply: plyRange) ;
VAR
   count: CARDINAL ;
BEGIN
   colors.magenta ; count := prettyMove (b, moves[m].cons) ;
   printf (" [") ;
   writeStatus (pstatus) ;
   printf (": %d]", plyPool^[ply].processors[m].result) ;
   colors.reset
END writeMagenta ;


(*
   writeCyan -
*)

PROCEDURE writeCyan (VAR b: Board; m: MoveRange; pstatus: status; ply: plyRange) ;
VAR
   count: CARDINAL ;
BEGIN
   colors.cyan ; count := prettyMove (b, moves[m].cons) ;
   printf (" [") ;
   writeStatus (pstatus) ;
   printf (": %d]", plyPool^[ply].processors[m].result) ;
   colors.reset
END writeCyan ;


(*
   writeStatus -
*)

PROCEDURE writeStatus (s: status) ;
BEGIN
   CASE s OF

   finished:  printf ("finished") |
   ready   :  printf ("ready") |
   running :  printf ("running")

   END
END writeStatus ;
*)


(*
   exploreSingleProcessor -
*)

PROCEDURE exploreSingleProcessor (VAR b: Board; m: MoveRange; ply: plyRange) ;
VAR
   result   : INTEGER ;
   pre, post: Board ;
BEGIN
   (*
   printf ("inc topCount %d\n", plyPool^[ply].topCount) ;
   *)
   INC (plyPool^[ply].topCount) ;
   (*
   stop1 ;
   printf ("single processor: ply %d, move %d  topCount %d\n", ply, m, plyPool^[ply].topCount) ;
   *)
   IF Stress
   THEN
      pre := b ;
      pushBoard (stringBoard (b)) ;
      executeForward (b, turn, m) ;     (* apply constructor.  *)
      post := b ;
      executeBackward (b, turn, m) ;    (* apply deconstructor.  *)
      assertPopBoard (stringBoard (b), pre, post, b, moves[m].cons, moves[m].decons)
   END ;
   executeForward (b, turn, m) ;
   result := exploreBoardAlphaBetaSingle (b, opposite (turn), plyPool^[ply].best) ;
   plyPool^[ply].best := rememberBest (ply, turn, result, m) ;
(*
   plyPool^[ply].processors[m].result := result ;
   plyPool^[ply].processors[m].pstatus := finished ;
*)
   executeBackward (b, turn, m) ;
   (* top (ply) *)
END exploreSingleProcessor ;


(*
   exploreMultiProcessor -
*)

PROCEDURE exploreMultiProcessor (VAR b: Board; m: MoveRange; ply: plyRange) ;
VAR
   mix   : INTEGER ;
   best,
   result: INTEGER ;
BEGIN
   wait (plyPool^[ply].plyMutex) ;
   best := plyPool^[ply].best ;
   signal (plyPool^[ply].plyMutex) ;
   (* the child process does the work.  *)
   totalMoveCount := 0 ;
   executeForward (b, turn, m) ;
   result := exploreBoardAlphaBetaMulti (b, opposite (turn), best, ply) ;
   printf ("child: %d deliver result: %d\n", getpid (), result) ;
   IF debugMultiProc
   THEN
      printf ("child: %d deliver result: %d\n", getpid (), result)
   END ;
   mix := VAL (INTEGER, m) ;
   mailbox.send (plyPool^[ply].barrier, result, mix, totalMoveCount)
END exploreMultiProcessor ;


(*
   distributeMove -
*)

PROCEDURE distributeMove (VAR b: Board; m: MoveRange; ply: plyRange) ;
VAR
   pid: INTEGER ;
BEGIN
   printf ("source waiting for core to be available\n");
   multiprocessor.wait (processorAvailable);
   printf (" ... available\n");
   pid := multiprocessor.fork ();
   IF pid = 0
   THEN
      exploreMultiProcessor (b, m, ply) ;
      multiprocessor.signal (processorAvailable) ;
      exit (0)
   END
END distributeMove ;


(*
   minScoreColour -

*)

PROCEDURE minScoreColour (c: Colour) : INTEGER ;
BEGIN
   IF c = white
   THEN
      RETURN MinScore
   ELSE
      RETURN MaxScore
   END
END minScoreColour ;


(*
   maxScoreColour -

*)

PROCEDURE maxScoreColour (c: Colour) : INTEGER ;
BEGIN
   IF c = white
   THEN
      RETURN MaxScore
   ELSE
      RETURN MinScore
   END
END maxScoreColour ;


(*
   processorMove -
*)
(*
PROCEDURE processorMove (VAR b: Board; m: MoveRange; ply: plyRange) : BOOLEAN ;
VAR
   p: CARDINAL ;
BEGIN
   p := 0 ;
   WHILE p < maxProcessors DO
      IF plyPool^[ply].processors[p].move = m
      THEN
         CASE plyPool^[ply].processors[p].pstatus OF

         finished:  writeGreen (b, m, finished, ply) |
         ready   :  writeYellow (b, m, ready) |
         running :  writeRed (b, m) ;
                    printf ("  [running] pid %d", plyPool^[ply].processors[p].childPid)

         END ;
         RETURN TRUE
      END ;
      INC (p)
   END ;
   RETURN FALSE
END processorMove ;


(*
   topColorMove - moves coloured green indicate complete.  A green colour
                  can occur when other moves are being explored and also
                  if there are no more moves to be explored.
*)

PROCEDURE topColorMove (VAR b: Board; m: MoveRange; ply: plyRange) ;
BEGIN
   (*
   printf ("plyPool^[ply].topCount = %d, nextProcessor = %d, m = %d\n",
           plyPool^[ply].topCount, plyPool^[ply].nextProcessors, m);
   *)
   IF m < plyPool^[ply].topCount
   THEN
      CASE plyPool^[ply].processors[m].pstatus OF

      finished:  writeGreen (b, m, finished, ply) |
      ready   :  writeCyan (b, m, ready, ply) |
      running :  writeRed (b, m)

      END
   ELSE
      (* otherwise we display the move with another colour.  *)
      CASE moves[m].kind OF

      mov:  writeCyan (b, m, ready, ply) ; printf ("  mov") |
      tak:  writeMagenta (b, m, ready, ply) ; printf ("  tak") |
      nop:  writeBlue (b, m, ready, ply) ;  printf ("  nop  [%d] ", m)

      END
   END ;
   printf ("\n")
END topColorMove ;


(*
   top - a pseudo ps (top) utility in chess shell.
*)

PROCEDURE top (ply: plyRange) ;
BEGIN
   colors.clear ;
   colors.home ;
   (*
   stop ;
   printf ("Ply: %d, Chess system processors:  %d,  Running chess processors %d,  Available chess processors: %d\n",
            ply, maxProcessors, plyPool^[ply].childrenActive) ;
   *)
   foreachMoveDo (currentBoard, topFrame.movePtr, movePtr, ply, topColorMove)
END top ;


(*
   primeTop -
*)

PROCEDURE primeTop (fd: FrameDescriptor; ply: plyRange) ;
VAR
   m: [0..MaxPlyMoves] ;
BEGIN
   plyPool^[ply].topCount := 0 ;
   topFrame := fd ;
   FOR m := 0 TO MaxPlyMoves DO
      plyPool^[ply].processors[m].result := 0 ;
      plyPool^[ply].processors[m].pstatus := ready ;
      plyPool^[ply].processors[m].childPid := 0 ;
      plyPool^[ply].processors[m].move := 0
   END
END primeTop ;
*)


(*
   collectMove - parent waits for a child to deliver a result.
*)

PROCEDURE collectMove (VAR b: Board; m: MoveRange; ply: plyRange) ;
VAR
   mix, result, noMoves: INTEGER ;
BEGIN
   (* remember that m is irrelevant as the children can finish in any order.  *)
   IF debugMultiProc
   THEN
      printf ("collectMove\n")
   END ;
   WITH plyPool^[ply] DO
      mailbox.rec (barrier, result, mix, noMoves) ;
      wait (plyMutex) ;
      best := rememberBest (ply, plyTurn, result, VAL (MoveRange, mix)) ;
      signal (plyMutex)
   END
END collectMove ;


(*
   distributeSearch -
*)

PROCEDURE distributeSearch (fd: FrameDescriptor; turn: Colour; alpha, beta: INTEGER) : INTEGER ;
VAR
   pid,
   best: INTEGER ;
BEGIN
   (* primeTop (fd, topPoolId) ; *)
   totalMoveCount := 0 ;
   IF maxProcessors > 1
   THEN
      (* parallel explore.  *)
      (*
       *  here we create a source and sink process, the source
       *  continually forks children one for every move, providing
       *  a processor is available.  The sink collects the
       *  results and ultimately returns the best move.
       *)
      pid := multiprocessor.fork () ;
      IF pid = 0
      THEN
         (* child is the source which spawns each move on a separate core.  *)
         foreachMoveDo (currentBoard, fd.movePtr, movePtr, topPoolId, distributeMove) ;
         exit (0)
      ELSE
         (* and the parent must wait for all the moves to be sent by the children.  *)
         foreachMoveDo (currentBoard, fd.movePtr, movePtr, topPoolId, collectMove)
         (* all children have completed from this point onwards.  *)
      END
   ELSE
      (* sequential explore.  *)
      foreachMoveDo (currentBoard, fd.movePtr, movePtr, topPoolId, exploreSingleProcessor)
   END ;
   best := plyPool^[topPoolId].best ;
   RETURN best
END distributeSearch ;


(*
   makeMove - computer makes a move for colour, col.
*)

PROCEDURE makeMove (fd: FrameDescriptor; VAR b: Board; col: Colour) : INTEGER ;
VAR
   result, singleResult: INTEGER ;
BEGIN
   IF verify AND (maxProcessors > 1)
   THEN
      calcScore (b) ;
      singleResult := distributeSearch (fd, col, MinScore, MaxScore) ;
      calcScore (b) ;
      result := distributeSearch (fd, col, MinScore, MaxScore) ;
      IF result = singleResult
      THEN
         printf ("single processor and multiprocessor verification passed\n")
      ELSE
         printf ("single processor and multiprocessor search algoriths generated different scores\n");
         printf ("single processor result: %d, multiprocessor result: %d\n", singleResult, result) ;
         exit (1)
      END ;
      RETURN result
   ELSIF verify
   THEN
      printf ("verify flag V only takes effect is more than one processor is allowed to explore the move\n")
   END ;
   calcScore (b) ;
   RETURN distributeSearch (fd, col, MinScore, MaxScore)
END makeMove ;


(*
   put -
*)

PROCEDURE put (b: WORD16) ;
BEGIN
   heap[heapPtr] := b ;
   INC (heapPtr)
END put ;


(*
   inputBoard -
*)

PROCEDURE inputBoard ;
BEGIN
   saveCurrentBoard ;
   printf ("enter board description\n");
   readBoard (currentBoard)
END inputBoard ;


(*
   boardFlagHelp -
*)

PROCEDURE boardFlagHelp ;
BEGIN
   printf ("enter flags and status:  [b|w] [KQkq-] number number\n");
   printf ("  where [b|w]  b means black to move, w white to move\n");
   printf ("        KQ     black can castle on king and queens side\n");
   printf ("        kq     white can castle on king and queens side\n");
   printf ("        -      neither side can castle\n");
   printf ("        number the first number is the 50 half move count\n");
   printf ("               ie the count of moves played since the last\n");
   printf ("               pawn was moved or piece taken\n");
   printf ("        number full move number\n")
END boardFlagHelp ;


(*
   skipSpaces -
*)

PROCEDURE skipSpaces ;
VAR
   ch: CHAR ;
BEGIN
   REPEAT
      ch := GetCh (inputFile)
   UNTIL NOT isWhite (ch) ;
   ch := PutCh (ch)
END skipSpaces ;


(*
   isDigit -
*)

PROCEDURE isDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch >= '0') AND (ch <= '9')
END isDigit ;


(*
   readCard8 -
*)

PROCEDURE readCard8 () : CARDINAL8 ;
VAR
   c : CARDINAL8 ;
   ch: CHAR ;
BEGIN
   c := 0 ;
   ch := GetCh (inputFile) ;
   WHILE isDigit (ch) DO
      c := c * 10 + VAL (CARDINAL8, ORD (ch) - ORD ('0')) ;
      ch := GetCh (inputFile)
   END ;
   ch := PutCh (ch) ;
   RETURN c
END readCard8 ;


(*
   inputBoardFlags -
*)

PROCEDURE inputBoardFlags ;
BEGIN
   readBoardFlags (currentBoard)
END inputBoardFlags ;


(*
   readBoardFlags - read the board flags from the format:
                    [b|w] [KQkq-] number number.
*)

PROCEDURE readBoardFlags (VAR b: Board) ;
VAR
   ch: CHAR ;
BEGIN
   skipSpaces ;
   ch := GetCh (inputFile) ;
   IF (ch # lf) AND (ch # cr)
   THEN
      CASE ch OF

      'b':  turn := black |
      'w':  turn := white

      ELSE
         printf ("illegal colour specification\n") ;
         RETURN
      END ;
      skipSpaces ;
      b.cflags := cFlags {} ;
      ch := GetCh (inputFile) ;
      WHILE (ch # '-') AND (NOT isWhite (ch)) DO
         CASE ch OF

         'k':  INCL (b.cflags, wks) |
         'q':  INCL (b.cflags, wqs) |
         'K':  INCL (b.cflags, bks) |
         'Q':  INCL (b.cflags, bqs)

         ELSE
            printf ("illegal castle flag specification\n") ;
            RETURN
         END ;
         ch := GetCh (inputFile) ;
      END ;
      skipSpaces ;
      b.noTake := readCard8 () ;
      skipSpaces ;
      b.fullMoveNo := readCard8 ()
   END
END readBoardFlags ;


(*
   readBoard -
*)

PROCEDURE readBoard (VAR b: Board) ;
VAR
   r, c: CARDINAL8 ;
BEGIN
   emptyBoard (b) ;
   displayBoard (b) ;
   printf ("enter 64 characters of board description\n");
   r := BoardSize ;
   REPEAT
      DEC (r) ;
      c := 0 ;
      WHILE c<BoardSize DO
         readSquare (b, r, c) ;
         INC (c)
      END
   UNTIL r=0
END readBoard ;


(*
   isWhite -
*)

PROCEDURE isWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch = ' ') OR (ch = tab)
END isWhite ;


(*
   skipSpacesAndExtra -
*)

PROCEDURE skipSpacesAndExtra ;
VAR
   ch: CHAR ;
BEGIN
   REPEAT
      ch := GetCh (inputFile)
   UNTIL (NOT isWhite (ch)) AND (ch # '+') AND
         (ch # '-') AND (ch # '|') AND
         (ch # lf) AND (ch # cr) ;
   ch := PutCh (ch)
END skipSpacesAndExtra ;


(*
   readSquare -
*)

PROCEDURE readSquare (VAR b: Board; r, c: CARDINAL8) ;
VAR
   ch: CHAR ;
BEGIN
   skipSpacesAndExtra ;
   ch := GetCh (inputFile) ;
   CASE ch OF

   '.':  |
   'p':  addPawn (b, black, c, r) |
   'n':  addKnight (b, black, c, r) |
   'b':  addBishop (b, black, c, r) |
   'r':  addRook (b, black, c, r) |
   'q':  addQueen (b, black, c, r) |
   'k':  addKing (b, black, c, r) |
   'P':  addPawn (b, white, c, r) |
   'N':  addKnight (b, white, c, r) |
   'B':  addBishop (b, white, c, r) |
   'R':  addRook (b, white, c, r) |
   'Q':  addQueen (b, white, c, r) |
   'K':  addKing (b, white, c, r) |
   'e':  addEnpassent (b, black, c, r) |
   'E':  addEnpassent (b, white, c, r)

   ELSE
      printf ("unknown character in board description '%c'\n", ch)
   END
END readSquare ;


(*
   test -
*)

PROCEDURE test ;
VAR
   b: Board ;
   h: HeapRange ;
BEGIN
   initBoard (b) ;
   displayBoard (b) ;
   h := heapPtr ;
   genMoves (b, white) ;
   foreachMoveDo (b, h, heapPtr, unusedPlyHeap, dumpMicrocode)
END test ;


(*
   toggleDebug -
*)

PROCEDURE toggleDebug ;
BEGIN
   debug := NOT debug
END toggleDebug ;


(*
   getDebug - return the debug flag.
*)

PROCEDURE getDebug () : BOOLEAN ;
BEGIN
   RETURN debug
END getDebug ;


(*
   toggleVerbose -
*)

PROCEDURE toggleVerbose ;
BEGIN
   verbose := NOT verbose
END toggleVerbose ;


(*
   toggleGroff -
*)

PROCEDURE toggleGroff ;
BEGIN
   groff := NOT groff
END toggleGroff ;


(*
   toggleVerify -
*)

PROCEDURE toggleVerify ;
BEGIN
   verify := NOT verify
END toggleVerify ;


(*
   prettyList - displays the moves in a standard format.
*)

PROCEDURE prettyList (VAR b: Board; m: MoveRange; ply: plyRange) ;
VAR
   count: HeapRange ;
BEGIN
   printf ("%d   ", m) ;
   count := prettyMove (b, moves[m].cons) ;
   printf ("\n")
END prettyList ;


(*
   saveFrame -
*)

PROCEDURE saveFrame () : FrameDescriptor ;
VAR
   fd: FrameDescriptor ;
BEGIN
   fd.heapPtr := heapPtr ;
   fd.movePtr := movePtr ;
   RETURN fd
END saveFrame ;


(*
   restoreFrame -
*)

PROCEDURE restoreFrame (fd: FrameDescriptor) ;
BEGIN
   heapPtr := fd.heapPtr ;
   movePtr := fd.movePtr ;
END restoreFrame ;


(*
   listMoves -
*)

PROCEDURE listMoves ;
VAR
   fd: FrameDescriptor ;
BEGIN
   fd := saveFrame () ;
   genMoves (currentBoard, turn) ;
   foreachMoveDo (currentBoard, fd.movePtr, movePtr, unusedPlyHeap, prettyList) ;
   restoreFrame (fd)
END listMoves ;


(*
   listMicrocodeMoves -
*)

PROCEDURE listMicrocodeMoves ;
VAR
   fd: FrameDescriptor ;
BEGIN
   fd := saveFrame () ;
   genMoves (currentBoard, turn) ;
   foreachMoveDo (currentBoard, fd.movePtr, movePtr, unusedPlyHeap, dumpMoves) ;
   restoreFrame (fd)
END listMicrocodeMoves ;


(*
   environment - display the user settings.
*)

PROCEDURE environment ;
BEGIN
   IF prioritiseTime
   THEN
      printf ("time is prioritised, ")
   END ;
   printf ("time allowed: %d seconds\n", timeAllowed) ;
   IF limitPlies
   THEN
      printf ("depth is limited to: %d plies\n", maxPlies)
   END ;
   printf ("processors available to chess shell: %d\n", maxProcessors) ;
   IF verify
   THEN
      printf ("single and multiprocessor verify is enabled\n")
   END ;
   IF debug
   THEN
      printf ("debug is enabled\n")
   END ;
   IF verbose
   THEN
      printf ("verbose is enabled\n")
   END
END environment ;


(*
   setMaxTime -
*)

PROCEDURE setMaxTime (secs: CARDINAL) ;
BEGIN
   prioritiseTime := (secs # 0) ;
   timeAllowed := secs
END setMaxTime ;


(*
   setMaxPlies -
*)

PROCEDURE setMaxPlies (plies: CARDINAL) ;
BEGIN
   limitPlies := (plies # 0) ;
   maxPlies := plies ;
   printf ("search depth set at: %d plies\n", plies)
END setMaxPlies ;


(*
   findMateIn -
*)

PROCEDURE findMateIn (plies: CARDINAL) ;
BEGIN
   setMaxPlies (plies) ;
   prioritiseTime := FALSE
END findMateIn ;


(*
   whiteMove -
*)

PROCEDURE whiteMove ;
BEGIN
   turn := white ;
   printTurn
END whiteMove ;


(*
   blackMove -
*)

PROCEDURE blackMove ;
BEGIN
   turn := black ;
   printTurn
END blackMove ;


(*
   printTurn -
*)

PROCEDURE printTurn ;
BEGIN
   IF turn = white
   THEN
      printf ("white to make a move\n")
   ELSE
      printf ("black to make a move\n")
   END
END printTurn ;


(*
   selectMove -
*)

PROCEDURE selectMove (i: CARDINAL) : BOOLEAN ;
VAR
   fd   : FrameDescriptor ;
   newpc: CARDINAL ;
BEGIN
   fd := saveFrame () ;
   genMoves (currentBoard, turn) ;
   IF i < movePtr - fd.movePtr
   THEN
      printf ("move selected: ");
      newpc := prettyMove (currentBoard, moves[i + fd.movePtr].cons) ;
      printf ("\n");
      executeForward (currentBoard, turn, i + fd.movePtr) ;
      turn := opposite (turn) ;
      printTurn ;
      restoreFrame (fd) ;
      RETURN TRUE
   ELSE
      restoreFrame (fd) ;
      RETURN FALSE
   END
END selectMove ;


(*
   chooseMove -
*)

PROCEDURE chooseMove (VAR mix: MoveRange; from, to: CARDINAL8; p: PieceT) : BOOLEAN ;
VAR
   fd       : FrameDescriptor ;
   src, dest: CARDINAL8 ;
   i        : MoveRange ;
BEGIN
   fd := saveFrame () ;
   genMoves (currentBoard, turn) ;
   i := fd.movePtr ;
   WHILE i < movePtr DO
      getSrcDestColour (currentBoard, moves[i].cons, src, dest, turn) ;
      (* printf ("%d, %d  ==  %d, %d\n", from, to, src, dest); *)
      IF (src = from) AND (dest = to)
      THEN
         mix := i ;
         restoreFrame (fd) ;
         RETURN TRUE
      END ;
      INC (i)
   END ;
   restoreFrame (fd) ;
   RETURN FALSE
END chooseMove ;


(*
   ord8 -
*)

PROCEDURE ord8 (ch: CHAR) : CARDINAL8 ;
BEGIN
   RETURN VAL (CARDINAL8, ch)
END ord8 ;


(*
   convertInput - convert input string, s, into from, to, p.  TRUE is returned if
                  the conversion contains legal characters.
*)

PROCEDURE convertInput (s: String; VAR from, to: CARDINAL8; VAR p: PieceT) : BOOLEAN ;
VAR
   x   : CARDINAL8 ;
   i, n: CARDINAL ;
   ch  : CHAR ;
BEGIN
   p := none ;
   n := Length (s) ;
   i := 0 ;
   IF EqualArray (s, "o-o")
   THEN
      from := WhiteKing ;
      to := CornerBotLeft ;
      RETURN TRUE
   ELSIF EqualArray (s, "o-o-o")
   THEN
      from := WhiteKing ;
      to := CornerBotRight ;
      RETURN TRUE
   ELSIF EqualArray (s, "O-O")
   THEN
      from := BlackKing ;
      to := CornerTopLeft ;
      RETURN TRUE
   ELSIF EqualArray (s, "O-O-O")
   THEN
      from := BlackKing ;
      to := CornerTopRight ;
      RETURN TRUE
   ELSIF n >= 4
   THEN
      ch := CAP (char (s, i)) ;
      IF (ch >= 'A') AND (ch <= 'H')
      THEN
         x := ord8 (ch) - ord8 ('A') ;
         INC (i) ;
         ch := char (s, i) ;
         IF (ch >= '1') AND (ch <= '8')
         THEN
            from := (ord8 (ch) - ord8 ('1')) * 8 + x ;
            INC (i)
         ELSE
            RETURN FALSE
         END ;
         ch := CAP (char (s, i)) ;
         IF (ch >= 'A') AND (ch <= 'H')
         THEN
            x := ord8 (ch) - ord8 ('A') ;
            INC (i) ;
            ch := char (s, i) ;
            IF (ch >= '1') AND (ch <= '8')
            THEN
               to := (ord8 (ch) - ord8 ('1')) * 8 + x ;
               INC (i)
            ELSE
               RETURN FALSE
            END ;
            IF i > 4
            THEN
               ch := char (s, i) ;
               CASE ch OF

               'n':  p := knight |
               'b':  p := bishop |
               'r':  p := rook |
               'q':  p := queen |
               ' ':

               ELSE
                  RETURN FALSE
               END
            END ;
            RETURN TRUE
         END
      END
   END ;
   RETURN FALSE
END convertInput ;


(*
   enterNextMove -
*)

PROCEDURE enterNextMove (s: String) ;
VAR
   p       : PieceT ;
   n, mix  : CARDINAL ;
   from, to: CARDINAL8 ;
BEGIN
   n := Length (s) ;
   IF n > 0
   THEN
      IF isDigit (char (s, 0))
      THEN
         mix := stoc (s) ;
         IF selectMove (mix)
         THEN
            printf ("move made\n");
            printBoard
         ELSE
            printf ("illegal move choice\n")
         END
      ELSE
         IF convertInput (s, from, to, p)
         THEN
            IF chooseMove (mix, to, from, p)
            THEN
               Assert (selectMove (mix)) ;
               printf ("move made\n");
               printBoard
            ELSE
               printf ("illegal move\n")
            END
         ELSE
            printf ("incorrectly specified move\n")
         END
      END
   END
END enterNextMove ;


(*
   printBoard -
*)

PROCEDURE printBoard ;
BEGIN
   displayBoard (currentBoard)
END printBoard ;


(*
   loadBoard -
*)

PROCEDURE loadBoard (n: CARDINAL) : BOOLEAN ;
VAR
   p: POINTER TO Board ;
BEGIN
   IF InBounds (history, n)
   THEN
      p := GetIndice (history, n) ;
      IF p#NIL
      THEN
         currentBoard := p^ ;
         RETURN TRUE
      END
   END ;
   RETURN FALSE
END loadBoard ;


(*
   performMakeNextMove -
*)

PROCEDURE performMakeNextMove ;
VAR
   score: INTEGER ;
   newpc: HeapRange ;
   fd   : FrameDescriptor ;
BEGIN
   topPoolId := newPlyPool (maxProcessors, turn,
                            minScoreColour (turn), maxScoreColour (turn),
                            maxProcessors > 1) ;
   fd := saveFrame () ;
   searchFlags := SearchFlagSet {} ;
   genMoves (currentBoard, turn) ;
   plyPool^[topPoolId].bestMove := MaxMoves ;
   plyPool^[topPoolId].best := minScoreColour (turn) ;
   IF movePtr = fd.movePtr
   THEN
      (* no moves, are we in check?  *)
      IF inCheck (currentBoard, turn)
      THEN
         (* yes, we have lost.  *)
         score := minScoreColour (turn)
      ELSE
         score := 0  (* it must be a draw.  *)
      END
   ELSIF movePtr = fd.movePtr + 1
   THEN
      plyPool^[topPoolId].bestMove := fd.movePtr ;  (* the only move available.  *)
      score := currentBoard.score
   ELSIF movePtr > fd.movePtr + 1
   THEN
      (* a choice of moves exist.  *)
      INCL (searchFlags, validsearch) ;
      score := makeMove (fd, currentBoard, turn) ;
      printFlags
   END ;
   (* remember score.  *)
   currentBoard.score := score ;
   (* now generate a message describing the move.  *)
   IF plyPool^[topPoolId].bestMove < MaxMoves
   THEN
      IF turn = white
      THEN
         printf ("whites move: ")
      ELSE
         printf ("blacks move: ")
      END ;
      newpc := prettyMove (currentBoard, moves[plyPool^[topPoolId].bestMove].cons) ;
      printf (" produces a score of %d\n", plyPool^[topPoolId].best) ;
      executeForward (currentBoard, turn, plyPool^[topPoolId].bestMove) ;
      saveCurrentBoard ;
      printBoard ;
      turn := opposite (turn) ;
      IF isCheckMate (currentBoard, turn)
      THEN
         IF turn = black
         THEN
            printf ("a win for white\n")
         ELSE
            printf ("a win for black\n")
         END
      ELSIF turn = white
      THEN
         printf ("white to make a move\n")
      ELSE
         printf ("black to make a move\n")
      END
   ELSE
      printf ("the game ends with ") ;
      IF score <= BlackWin
      THEN
         IF turn = black
         THEN
            printf ("a win for black\n")
         ELSE
            printf ("a loss for white\n")
         END
      ELSIF score >= WhiteWin
      THEN
         IF turn = white
         THEN
            printf ("a win for white\n")
         ELSE
            printf ("a loss for black\n")
         END
      ELSE
         printf ("a draw\n")
      END
   END ;
   restoreFrame (fd) ;
   topPoolId := killPlyPool (topPoolId)
END performMakeNextMove ;


(*
   makeNextMove - make a move on the side of, turn, on the currentBoard.
*)

PROCEDURE makeNextMove ;
BEGIN
   IF inCheck (currentBoard, opposite (turn))
   THEN
      IF turn = white
      THEN
         printf ("black king is already in check, cannot make a move\n")
      ELSE
         printf ("white king is already in check, cannot make a move\n")
      END
   ELSE
      performMakeNextMove
   END
END makeNextMove ;


(*
   setOutputFile -
*)

PROCEDURE setOutputFile (f: File) ;
BEGIN

END setOutputFile ;


(*
   setInputFile -
*)

PROCEDURE setInputFile (f: File) ;
BEGIN
   inputFile := f
END setInputFile ;


(*
   searchAssert -
*)

PROCEDURE searchAssert (s: String) ;
VAR
   i      : CARDINAL ;
   w      : String ;
   success: BOOLEAN ;
BEGIN
   success := TRUE ;
   i := 1 ;
   WHILE GetArg (s, i, w) DO
      IF EqualArray (w, "=")
      THEN
         IF NOT (forcedraw IN searchFlags)
         THEN
            printf ("user assert failed: the search did not find a forced draw\n") ;
            success := FALSE
         END
      END ;
      IF EqualArray (w, "w++")
      THEN
         IF NOT (forcewhitewin IN searchFlags)
         THEN
            printf ("user assert failed: the search did not find a forced win for white\n") ;
            success := FALSE
         END
      END ;
      IF EqualArray (w, "b++")
      THEN
         IF NOT (forceblackwin IN searchFlags)
         THEN
            printf ("user assert failed: the search did not find a forced win for black\n") ;
            success := FALSE
         END
      END ;
      IF EqualArray (w, "w+")
      THEN
         IF NOT inCheck (currentBoard, white)
         THEN
            printf ("user assert failed: white is not in check in the current board position\n") ;
            displayBoard (currentBoard) ;
            success := FALSE
         END
      END ;
      IF EqualArray (w, "b+")
      THEN
         IF NOT inCheck (currentBoard, black)
         THEN
            printf ("user assert failed: black is not in check in the current board position\n") ;
            displayBoard (currentBoard) ;
            success := FALSE
         END
      END ;
      INC (i) ;
      w := KillString (w)
   END ;
   IF success
   THEN
      printf ("all user asserts passed: %s\n", string (s))
   ELSIF validsearch IN searchFlags
   THEN
      printf ("terminating as user assert has failed\n") ;
      exit (1)
   END
END searchAssert ;


(*
   calcScoreColour -
*)

PROCEDURE calcScoreColour (VAR b: Board; c: Colour) : INTEGER ;
VAR
   value: INTEGER ;
   i    : CARDINAL8 ;
BEGIN
   value := 0 ;
   i := 0 ;
   WHILE i < b.pieces[c].nPawns DO
      IF b.pieces[c].pawn[i].xy < UnusedSquare
      THEN
         INC (value, pieceValue[pawn])
      END ;
      INC (i)
   END ;
   i := 0 ;
   WHILE i < b.pieces[c].nKnights DO
      IF b.pieces[c].knight[i].xy < UnusedSquare
      THEN
         INC (value, pieceValue[knight])
      END ;
      INC (i)
   END ;
   i := 0 ;
   WHILE i < b.pieces[c].nWBishops DO
      IF b.pieces[c].wbishop[i].xy < UnusedSquare
      THEN
         INC (value, pieceValue[bishop])
      END ;
      INC (i)
   END ;
   i := 0 ;
   WHILE i < b.pieces[c].nBBishops DO
      IF b.pieces[c].bbishop[i].xy < UnusedSquare
      THEN
         INC (value, pieceValue[bishop])
      END ;
      INC (i)
   END ;
   i := 0 ;
   WHILE i < b.pieces[c].nRooks DO
      IF b.pieces[c].rook[i].xy < UnusedSquare
      THEN
         INC (value, pieceValue[rook])
      END ;
      INC (i)
   END ;
   i := 0 ;
   WHILE i < b.pieces[c].nQueens DO
      IF b.pieces[c].queen[i].xy < UnusedSquare
      THEN
         INC (value, pieceValue[queen])
      END ;
      INC (i)
   END ;
   RETURN value
END calcScoreColour ;


(*
   calcScore  -
*)

PROCEDURE calcScore  (VAR b: Board) ;
BEGIN
   b.score := calcScoreColour (b, white) - calcScoreColour (b, black)
END calcScore  ;


(*
   initFreePlyPool -
*)

PROCEDURE initFreePlyPool ;
VAR
   p: plyRange ;
BEGIN
   FOR p := MIN (plyArrayRange) TO MAX (plyArrayRange) DO
      plyPool^[p].initialised := FALSE ;   (* semaphores have not been initialised.  *)
      plyPool^[p].inuse := FALSE ;  (* not in use yet.  *)
      IF p < MAX (plyArrayRange)
      THEN
         plyPool^[p].nextFree := p+1
      ELSE
         plyPool^[p].nextFree := 0
      END
   END ;
   freePlyPool := MIN (plyArrayRange)
END initFreePlyPool ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   IF m2config.MULTIPROCESSOR
   THEN
      plyPool := mailbox.initSharedMemory (SIZE (plyArray)) ;
      mutexPlyPool := multiprocessor.initSem (1) ;
      initFreePlyPool
   ELSE
      (* Create plyPool from heap memory.  *)
      ALLOCATE (plyPool, SIZE (plyArray)) ;
      initFreePlyPool  (* We are only going to use the first plyPool.  *)
   END ;
   stackPtr := 0 ;
   maxPlies := DefaultPlies ;
   verbose := FALSE ;
   debug := FALSE ;
   groff := FALSE ;
   verify := FALSE ;
   prioritiseTime := TRUE ;
   limitPlies := TRUE ;
   timeAllowed := 10 ;   (* initially 10 seconds/move  *)
   heapPtr := 0 ;
   movePtr := 0 ;
   boardNo := 1 ;
   turn := white ;
   history := InitIndex (1) ;
   initBoard (currentBoard) ;
   inputFile := StdIn ;
   Assert (SIZE (Instruction)=2) ;
   Assert (SIZE (InstructionAdd)=2) ;
   Assert (SIZE (InstructionSub)=2) ;
   Assert (SIZE (InstructionMove)=2) ;
   Assert (SIZE (InstructionFlags)=2) ;
   (* None, Pawn, Shadow, Knight, Bishop, Rook, Queen, King.  *)
   pieceValue := PieceValue {0, 1 * ScalePiece, 0, 3 * ScalePiece, 3 * ScalePiece, 5 * ScalePiece, 9 * ScalePiece, 0} ;
   IF m2config.MULTIPROCESSOR
   THEN
      maxProcessors := multiprocessor.maxProcessors () ;
      processorAvailable := multiprocessor.initSem (maxProcessors)
   ELSE
      maxProcessors := 1
   END ;
   colors.red ;
   printf ("%d processor chess engine\n", maxProcessors) ;
   colors.reset ;
   saveCurrentBoard
END init ;


BEGIN
   init
END chessBoard.
(*
 * Local variables:
 *  compile-command: "gm2 -fm2-g -fiso -g -c chessBoard.mod"
 * End:
 *)
