IMPLEMENTATION MODULE colors ;

FROM libc IMPORT printf ;


VAR
   enabled: BOOLEAN ;


(*
   enable - turn on/off production of color terminal escape strings
            associated with each procedure.
*)

PROCEDURE enable (value: BOOLEAN) ;
BEGIN
   enabled := value
END enable ;


(*
   red -
*)

PROCEDURE red ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[31m")
   END
END red ;


(*
   green -
*)

PROCEDURE green ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[32m")
   END
END green ;


(*
   yellow -
*)

PROCEDURE yellow ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[33m")
   END
END yellow ;


(*
   blue -
*)

PROCEDURE blue ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[34m")
   END
END blue ;


(*
   magenta -
*)

PROCEDURE magenta ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[35m")
   END
END magenta ;


(*
   cyan -
*)

PROCEDURE cyan ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[36m")
   END
END cyan ;


(*
   white -
*)

PROCEDURE white ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[37m")
   END
END white ;


(*
   reset -
*)

PROCEDURE reset ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[0m")
   END
END reset ;


(*
   clear -
*)

PROCEDURE clear ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[2J")
   END
END clear ;


(*
   home -
*)

PROCEDURE home ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[0;0H")
   END
END home ;


(*
   pos - position cursor at location, x, y on the screen.
         0, 0 is top left corner.
*)

PROCEDURE pos (x, y: CARDINAL) ;
BEGIN
   IF enabled
   THEN
      printf ("\x1B[%d;%dH", y, x)
   END
END pos ;


END colors.
