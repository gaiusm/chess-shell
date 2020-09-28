# chess-shell

Chess shell allows you to play a game of chess or explore chess
problems (such as mate in three etc).  It implements the rules of
chess and evaluates the board position using a parallel alpha beta
algorithm.  Chess shell exploits the number of cores available unless
overridden by the user.  It uses a microcode and virtual processor to
apply and retract a move.

At present there are a number of bugs in chess-shell.  However it
does pass the limited number of regression tests.

It should work fine with King/King and any number of Rooks.  There
are bugs in the bishop related code and queen related code (I think).
At a guess the bugs are somewhere with the diagonal handling of movement
(but as with most bug hunting - a guess could be wrong).
