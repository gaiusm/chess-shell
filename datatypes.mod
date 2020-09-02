MODULE datatypes ;

FROM Assertion IMPORT Assert ;
FROM SYSTEM IMPORT BYTE ;

CONST
   MaxSquares      = 64 ;
   BoardSize       =  8 ;

TYPE
   Colour          = (white, black) ;
   PieceT          = (none, pawn, shadow, knight, bishop, rook, queen, king) ;
   Squares         = [0..MaxSquares-1] ;
   PieceIndex      = [0..MAX (KingRange)] ;

   PawnRange       = [0..7] ;
   ShadowRange     = [8..8] ;
   KnightRange     = [9..18] ;
   WBishopRange    = [19..27] ;
   BBishopRange    = [28..36] ;
   RookRange       = [37..46] ;
   QueenRange      = [47..55] ;
   KingRange       = [56..56] ;

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
                        pieceKind: PieceT ;      (* 3 bits *)
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

BEGIN
   Assert (SIZE (Instruction)=2) ;
   Assert (SIZE (InstructionAdd)=2) ;
   Assert (SIZE (InstructionSub)=2) ;
   Assert (SIZE (InstructionMove)=2) ;
   Assert (SIZE (InstructionFlags)=2)
END datatypes.
