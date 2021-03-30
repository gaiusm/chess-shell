d
#
#  chess shell test input for a mate in 2 moves
#  or 4 half moves
#
h
c
p
l
s
......k.
........
R.......
.......K
........
........
........
........
p
# when this uses a parallel search the assert below fails
c 4
V
c 12  # will fail!
t 0
m 5
e
j
n
# we should have seen the check mate now
A w++
n
n
n
n
n
n
x
