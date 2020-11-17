#!/usr/bin/env python3

import pexpect, sys

# debugging = False
debugging = True


class shell:
    def __init__ (self, filename = None):
        self.child = pexpect.spawn ('./chess-shell')
        self._sync_no = 0
        if debugging:
            self.child.logfile = sys.stdout.buffer
        if not (filename is None):
            self.child.sendline ('P ' + filename)
        self._get_prompt ()
    def _get_prompt (self):
        self.child.sendline ('E sync%d' % (self._sync_no))
        i = 0
        while i == 0:
            i = self.child.expect ([pexpect.TIMEOUT, "sync%d" % (self._sync_no)])
        self._sync_no += 1
    def get_board (self):
        self._get_prompt ()
        self.child.sendline ('p')
        # top label
        i = self.child.expect ([pexpect.TIMEOUT, 'a b c d e f g h'])
        if i == 1:
            # bottom label
            i = self.child.expect ([pexpect.TIMEOUT, 'a b c d e f g h'])
            text = self.child.before
            text = text.decode("utf-8")
            text = text.replace (" ", "")
            board = ""
            for w in text.split ("|"):
                if (len (w) == 8) and (w.find ("\n") == -1):
                    board += w
            return board
    def get_legal_moves (self):
        self._get_prompt ()
        self.child.sendline ('l')
        self._get_prompt ()
        moves = self.child.before
        text = moves.decode("utf-8")
        print ("text = ", text)
        list_of_moves = []
        count = 0
        for t in text.split ('\n'):
            t = t.lstrip ()
            t = t.rstrip ()
            if (t != '') and (len (t) > 4) and (t[:4] != "sync"):
                w = t.split ()
                if (len (w) > 1) and (int (w[0]) == count):
                    count += 1
                    for i in w[1:]:
                        if len (i) > 0:
                            list_of_moves += [i]
        return list_of_moves
    def make_move (self, move_number):
        self._help ()
        self._get_prompt ()
        self.child.sendline ('k %d' % (move_number))
    def _help (self):
        self._get_prompt ()
        self.child.sendline ('h')
        self.child.expect ([pexpect.TIMEOUT, 'P inputfile'])
        print ("seen help")


def display (message, board):
    print (message)
    for line_no in range (8):
        print (board[line_no * 8:(line_no + 1) * 8])


def testShell ():
    s = shell ("../chess-shell/examples/rooks.cs")
    display ("The Board", s.get_board ())
    print ("The Moves", s.get_legal_moves ())
    s.make_move (6)
    display ("The Board", s.get_board ())
    print ("The Moves", s.get_legal_moves ())


testShell ()
