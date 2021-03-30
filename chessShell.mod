(* Copyright (C) 2015-2020 Free Software Foundation, Inc. *)
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

MODULE chessShell ;

FROM SYSTEM IMPORT ADDRESS ;
FROM FIO IMPORT File, StdIn, Close, EOF ;
FROM stdio IMPORT fflush ;
FROM PushBackInput IMPORT Open, GetCh, PutStr, Error ;
FROM libc IMPORT printf, exit ;
FROM ASCII IMPORT lf ;
FROM SCmdArgs IMPORT GetArg ;
FROM SFIO IMPORT ReadS, Exists, OpenToRead ;
FROM StringConvert IMPORT stoc ;

FROM chessBoard IMPORT printBoard, whiteMove, blackMove, loadBoard, toggleDebug, toggleVerbose, toggleGroff,
                       findMateIn, setMaxTime, makeNextMove, listMoves, inputBoard, setInputFile, getDebug,
                       setMaxProcessors, listMicrocodeMoves, environment, boardFlagHelp, printStatus,
                       enterNextMove, searchAssert, toggleVerify ;

FROM DynamicStrings IMPORT String, InitString, ConCatChar, EqualArray, Dup, string, Length,
                           RemoveWhitePrefix, RemoveWhitePostfix, KillString, Index, Slice, Mark, char ;

IMPORT SArgs, colors ;


VAR
   debug,
   fileprompt,
   stdinprompt:  BOOLEAN ;


(*
   displayHelp -
*)

PROCEDURE displayHelp ;
BEGIN
   printf ("shell commands:\n");
   printf ("a number          : apply a move, the number is the move number found in the list\n");
   printf ("b                 : black to make a move\n");
   printf ("c [number]        : limit the number of processors to number\n");
   printf ("                  : if no number is given then all processors are used\n");
   printf ("d                 : toggle debugging\n");
   printf ("e                 : display the settings (environment)\n");
   printf ("f i [fen|raw]     : input board format in raw or fen format\n");
   printf ("f o [fen|raw]     : output board format in raw or fen format\n");
   printf ("g                 : all output will be in groff typeset format\n");
   printf ("h                 : help\n");
   printf ("i inputfile       : read shell commands from inputfile\n");
   printf ("j                 : toggle colour output\n");
   printf ("k move            : human makes a move, move is [a-h][1-8][a-h][1-8][nbrq] or o-o or o-o-o\n");
   printf ("l                 : list all legal moves playable from the current board\n");
   printf ("m number          : search for mate in number moves\n");
   printf ("n                 : computer makes the next move\n");
   printf ("o outputfile      : send all the output to outputfile\n");
   printf ("p                 : print current board\n");
   printf ("q boardflags      : set the board flags which must be in the format\n");
   printf ("                    [b|w] [KQkq-] number number  (use q help for more detail).\n");
   printf ("r [number]        : reset board to board number.\n");
   printf ("                  : if no number is given then it is set to the start position.\n");
   printf ("s                 : set board to the position given in the next eight lines\n");
   printf ("t time            : set the maximum time, default time is in seconds.\n");
   printf ("                  : time may be an integer followed by s or m or h.\n");
   printf ("u                 : print status of the board (castling status and move counts)\n");
   printf ("v                 : toggle verbose\n");
   printf ("w                 : white to make a move\n");
   printf ("x                 : exit\n");
   printf ("y                 : list moves encoded by internal microcode\n");
   printf ("z                 : revert previous move\n");
   printf ("A [=] [w++] [b++] : assert move search discovers forced draw,\n");
   printf ("                    forced white win and forced black win\n");
   printf ("P inputfile       : push inputfile and read shell commands.\n");
   printf ("                    Input reverts back to the previous file.\n");
   printf ("E echostring      : echostring to the output file or stream.\n");
   printf ("S                 : toggle prompt when stdin is used as input.\n");
   printf ("F                 : toggle prompt when a file is used as input.\n");
   printf ("V                 : verify single and multiprocessor scores are the same.\n");
   printf ("                  : this option runs the single and multiprocessor search algoriths\n");
   printf ("                  : and checks that the same score is calculated\n");
   printf ("                  : (there may be multiple moves with the same score though).\n");
END displayHelp ;


(*
   readS - returns a string after removing any comment on the right hand side.
*)

PROCEDURE readS () : String ;
VAR
   s: String ;
   i: INTEGER ;
BEGIN
   s := ReadS (inputFile) ;
   i := Index (s, '#', 0) ;
   IF i#-1
   THEN
      s := Slice (Mark(s), 0, i)
   END ;
   RETURN s
END readS ;


(*
   resetBoard -
*)

PROCEDURE resetBoard (s: String) ;
VAR
   i: String ;
   n: CARDINAL ;
BEGIN
   IF GetArg (s, 1, i)
   THEN
      n := stoc (i) ;
      IF loadBoard (n)
      THEN
         printf ("loaded board # %d\n", n)
      ELSE
         printf ("board # %d does not exist\n", n)
      END
   ELSIF loadBoard (1)
   THEN
      printf ("loaded board # 1\n")
   ELSE
      printf ("error cannot load board # 1\n")
   END
END resetBoard ;


(*
   enterMove -
*)

PROCEDURE enterMove (s: String) ;
VAR
   m: String ;
BEGIN
   IF GetArg (s, 1, m)
   THEN
      m := RemoveWhitePrefix (m) ;
      m := RemoveWhitePostfix (m) ;
      enterNextMove (m) ;
      m := KillString (m)
   ELSE
      printf ("expecting a move after the k command\n")
   END
END enterMove ;


(*
   applyMove -
*)

PROCEDURE applyMove (s: String) ;
VAR
   m: String ;
BEGIN
   IF GetArg (s, 1, m)
   THEN
      m := RemoveWhitePrefix (m) ;
      m := RemoveWhitePostfix (m) ;
      enterNextMove (m) ;
      m := KillString (m)
   ELSE
      printf ("expecting a number after the a command, to see the move numbers use 'l'.\n");
      printf ("Alternatively use 'k' to enter a move.\n")
   END
END applyMove ;


(*
   processMateIn -
*)

PROCEDURE processMateIn (s: String) ;
VAR
   i    : String ;
   plies: CARDINAL ;
BEGIN
   IF GetArg (s, 1, i)
   THEN
      plies := stoc (i) ;
      findMateIn (plies)
   ELSE
      printf ("the command m requires a number\n")
   END
END processMateIn ;


(*
   processMaxTime -
*)

PROCEDURE processMaxTime (s: String) ;
VAR
   seconds: CARDINAL ;
   i      : String ;
BEGIN
   seconds := 0 ;
   IF GetArg (s, 1, i) AND (Length (i) > 0)
   THEN
      IF char (i, -1) = 's'
      THEN
         i := Slice (Mark (i), 0, -1) ;
         seconds := stoc (i)
      ELSIF char (i, -1) = 'm'
      THEN
         i := Slice (Mark (i), 0, -1) ;
         seconds := stoc (i) * 60
      ELSIF char (i, -1) = 'h'
      THEN
         i := Slice (Mark (i), 0, -1) ;
         seconds := stoc (i) * 60 * 60
      ELSE
         seconds := stoc (i)
      END
   END ;
   IF seconds = 0
   THEN
      printf ("max time per move is disabled\n") ;
      setMaxTime (seconds)
   ELSE
      printf ("max time per move or query is set to: %d seconds\n", seconds) ;
      setMaxTime (seconds)
   END
END processMaxTime ;


(*
   sourceFrom -
*)

PROCEDURE sourceFrom (s: String) ;
VAR
   old: File ;
BEGIN
   IF EqualArray (s, '-')
   THEN
      old := inputFile ;
      inputFile := StdIn ;
      setInputFile (inputFile) ;
      shell ;
      inputFile := old ;
      setInputFile (inputFile) ;
   ELSIF Exists (s)
   THEN
      old := inputFile ;
      inputFile := OpenToRead (s) ;
      setInputFile (inputFile) ;
      shell ;
      Close (inputFile) ;
      inputFile := old ;
      setInputFile (inputFile)
   ELSE
      printf ("no such file: %s\n", string (s))
   END
END sourceFrom ;


(*
   processInputFile -
*)

PROCEDURE processInputFile (s: String) ;
VAR
   i: String ;
BEGIN
   IF GetArg (s, 1, i)
   THEN
      sourceFrom (i)
   ELSE
      printf ("a filename is expected after 'i'\n")
   END
END processInputFile ;


(*
   processOutputFile -
*)

PROCEDURE processOutputFile (s: String) ;
BEGIN

END processOutputFile ;


(*
   pushInputFile -
*)

PROCEDURE pushInputFile (s: String) ;
VAR
   i: String ;
BEGIN
   IF GetArg (s, 1, i)
   THEN
      sourceFrom (i)
   ELSE
      printf ("a filename is expected after 'P'\n")
   END
END pushInputFile ;


(*
   limitProcessors -
*)

PROCEDURE limitProcessors (s: String) ;
VAR
   i: String ;
   n: CARDINAL ;
   a: ADDRESS ;
BEGIN
   IF GetArg (s, 1, i)
   THEN
      n := stoc (i) ;
      IF n = 0
      THEN
         a := string (i) ;
         printf ("the processor limit must be a number >= 1 and not %s\n", a)
      ELSE
         setMaxProcessors (n)
      END ;
      i := KillString (i)
   ELSE
      setMaxProcessors (0)
   END
END limitProcessors ;


(*
   boardFlags -
*)

PROCEDURE boardFlags (s: String) ;
VAR
   i: String ;
BEGIN
   IF GetArg (s, 1, i)
   THEN
      IF EqualArray (i, 'help')
      THEN
         boardFlagHelp
      ELSE
         PutStr (i)
      END
   ELSE
      boardFlagHelp
   END ;
   i := KillString (i)
END boardFlags ;


(*
   echoString -
*)

PROCEDURE echoString (s: String) ;
VAR
   i: String ;
BEGIN
   IF GetArg (s, 1, i)
   THEN
      printf ("%s\n", i) ;
      i := KillString (i)
   END
END echoString ;


(*
   toggleBoolean -
*)

PROCEDURE toggleBoolean (VAR b: BOOLEAN; name: ARRAY OF CHAR) ;
VAR
   s: String ;
   a: ADDRESS ;
BEGIN
   s := InitString (name) ;
   a := string (s) ;
   IF b
   THEN
      printf ("%s is now off\n", a) ;
      b := FALSE
   ELSE
      printf ("%s is now on\n", a) ;
      b := TRUE
   END ;
   s := KillString (s)
END toggleBoolean ;


(*
   processString -
*)

PROCEDURE processString (s: String) : BOOLEAN ;
VAR
   w   : String ;
   a, b: ADDRESS ;
BEGIN
   s := RemoveWhitePrefix (s) ;
   s := RemoveWhitePostfix (s) ;
   IF (s#NIL) AND (NOT EqualArray (s, ''))
   THEN
      w := Dup (s) ;
      a := string (s) ;
      b := string (w) ;
      IF getDebug ()
      THEN
         printf ("commandline = %s\n", a)
      END ;
      IF GetArg (s, 0, w)
      THEN
         a := string (w) ;
         IF char (w, 0)='#'
         THEN
            (* ignore comment.  *)
         ELSIF EqualArray (w, 'e')
         THEN
            environment
         ELSIF EqualArray (w, 'h')
         THEN
            displayHelp
         ELSIF EqualArray (w, 'p')
         THEN
            printBoard
         ELSIF EqualArray (w, 'q')
         THEN
            boardFlags (s)
         ELSIF EqualArray (w, 'r')
         THEN
            resetBoard (s)
         ELSIF EqualArray (w, 'a')
         THEN
            applyMove (s)
         ELSIF EqualArray (w, 'w')
         THEN
            whiteMove
         ELSIF EqualArray (w, 'b')
         THEN
            blackMove
         ELSIF EqualArray (w, 'c')
         THEN
            limitProcessors (s)
         ELSIF EqualArray (w, 'd')
         THEN
            toggleDebug
         ELSIF EqualArray (w, 'k')
         THEN
            enterMove (s)
         ELSIF EqualArray (w, 'v')
         THEN
            toggleVerbose
         ELSIF EqualArray (w, 'm')
         THEN
            processMateIn (s)
         ELSIF EqualArray (w, 't')
         THEN
            processMaxTime (s)
         ELSIF EqualArray (w, 'n')
         THEN
            makeNextMove
         ELSIF EqualArray (w, 'o')
         THEN
            processOutputFile (s)
         ELSIF EqualArray (w, 'i')
         THEN
            processInputFile (s)
         ELSIF EqualArray (w, 'j')
         THEN
            colourEnabled := NOT colourEnabled ;
            IF colourEnabled
            THEN
               printf ("enabling colour\n")
            ELSE
               printf ("disabling colour\n")
            END ;
            colors.enable (colourEnabled)
         ELSIF EqualArray (w, 'l')
         THEN
            listMoves
         ELSIF EqualArray (w, 's')
         THEN
            inputBoard
         ELSIF EqualArray (w, 'u')
         THEN
            printStatus
         ELSIF EqualArray (w, 'g')
         THEN
            toggleGroff
         ELSIF EqualArray (w, 'x')
         THEN
            printf ("exiting chess shell\n") ;
            RETURN FALSE
         ELSIF EqualArray (w, 'y')
         THEN
            listMicrocodeMoves
         ELSIF EqualArray (w, 'A')
         THEN
            searchAssert (s)
         ELSIF EqualArray (w, 'P')
         THEN
            pushInputFile (s)
         ELSIF EqualArray (w, 'E')
         THEN
            echoString (s)
         ELSIF EqualArray (w, 'S')
         THEN
            toggleBoolean (stdinprompt, "stdin prompt")
         ELSIF EqualArray (w, 'F')
         THEN
            toggleBoolean (fileprompt, "file prompt")
         ELSIF EqualArray (w, 'V')
         THEN
            toggleVerify
         ELSE
            a := string (s) ;
            printf ("unknown command: %s\n", a);
         END
      END ;
      w := KillString (w) ;
      s := KillString (s)
   END ;
   RETURN TRUE
END processString ;


(*
   shell -
*)

PROCEDURE shell ;
VAR
   s: String ;
   o: ADDRESS ;
BEGIN
   REPEAT
      IF ((inputFile = StdIn) AND stdinprompt) OR ((inputFile # StdIn) AND fileprompt)
      THEN
         printf ("@ ")
      END ;
      fflush ;
      s := readS () ;
      o := string (s) ;
      (* printf ("@ %s\n", o); *)
   UNTIL EOF (inputFile) OR (NOT processString (s))
END shell ;


(*
   init -
*)

PROCEDURE init ;
VAR
   s: String ;
   i: CARDINAL ;
BEGIN
   inputFile := StdIn ;
   IF SArgs.Narg () = 1
   THEN
      sourceFrom (InitString ('-'))
   ELSE
      i := 1 ;
      WHILE SArgs.GetArg (s, i) DO
         sourceFrom (s) ;
         INC (i)
      END
   END
END init ;


VAR
   inputFile    : File ;
   colourEnabled: BOOLEAN ;
BEGIN
   fileprompt := FALSE ;
   stdinprompt := FALSE ;
   colourEnabled := FALSE ;
   colors.enable (colourEnabled) ;
   init
END chessShell.
(*
 * Local variables:
 *  compile-command: "gm2 -g -c chessShell.mod"
 * End:
 *)
