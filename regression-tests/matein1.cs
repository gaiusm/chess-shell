d
#
#  chess shell test input for a mate in 1 moves
#  white pawn takes rook promote to queen and check mate
#
h
c 1
p
l
s
.....r.k
....P...
......P.
........
........
........
......K.
........
q - 0 1
p
u
c 1
j
l
# turn off time restriction
t 0
# explore 1 move ahead (2 half moves or plies)
m 2
l
y
n
# assert that the win has been seen
A w++
x
