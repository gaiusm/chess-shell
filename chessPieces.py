#!/usr/bin/env python3

import chess, os, chess.svg, bres, PIL
import sys, getopt, chessShell

from PIL import Image

import pygame, touchgui, touchguipalate, touchguiconf
from touchgui import posX, posY
from pygame.locals import *

# display_width, display_height = 1920, 1080
display_width, display_height = 800, 600
display_width, display_height = 1920, 1080
full_screen = True
full_screen = False
toggle_delay = 250
light_square = (166, 124, 54)
dark_square  = (76, 47, 0)
high_dark_square  = (25, 25, 25)
high_light_square  = (51, 51, 51)
black = (0, 0, 0)
movement_pieces = []
fading_pieces = []
is_finished = False
max_delay = 10
max_velocity = max_delay
square_size = 0.1
move_src_square = None
move_dest_square = None


def myquit (name = None, tap = None):
    pygame.display.update ()  # need this to see the button pressed before we quit
    pygame.time.delay (toggle_delay * 2) #  delay program so we see the button change
    pygame.quit ()  #  now shutdown pygame
    quit ()  #  and shutdown python


def pos2key (pos):
    return "%c%c" % (int (pos[0] / touchgui.unitY (square_size)) + ord ('a') -1,
                     (8 - int (pos[1] / touchgui.unitY (square_size))) + ord ('1'))


def square_pressed (name, tap = None):
    global move_src_square, shell, is_finished

    print (name, "square_pressed")
    pos = pygame.mouse.get_pos ()
    key = pos2key (pos)
    print (key)
    #
    #  check for src move click, we can only have clicked on a piece which
    #  can be moved as we have frozen all other piece tiles.
    #
    assert (pieces[key] != None)
    move_src_square = key
    is_finished = True


#
#  dest_selected - the destination move square has been pressed.
#

def dest_selected (name, tap = None):
    global move_dest_square, is_finished
    pos = pygame.mouse.get_pos ()
    move_dest_square = pos2key (pos)
    print ("dest square selected =", move_dest_square)
    is_finished = True


def finished ():
    return is_finished


def imagedir (name):
    return os.path.join (touchguiconf.touchguidir, name)


def button_list (name):
    return [touchgui.image_gui (imagedir ("images/PNG/White/2x/%s.png") % (name)).white2grey (.5),
            touchgui.image_gui (imagedir ("images/PNG/White/2x/%s.png") % (name)).white2grey (.1),
            touchgui.image_gui (imagedir ("images/PNG/White/2x/%s.png") % (name)),
            touchgui.image_gui (imagedir ("images/PNG/White/2x/%s.png") % (name)).white2rgb (.1, .2, .4)]

#
#  buttons - create two buttons and return them as a list.
#

def buttons ():
    return [touchgui.image_tile (button_list ("power"),
                                 touchgui.posX (0.95), touchgui.posY (1.0),
                                 100, 100, myquit)]


def makePiece (name, size):
    os.system ("mkdir -p chessimages")
    p = chess.svg.piece (chess.Piece.from_symbol (name), size)
    f = open ('chessimages/%s.svg' % (name), 'w')
    f.write (p)
    f.close ()
    os.system ('convert -background none chessimages/%s.svg chessimages/%s.png' % (name, name))


def makePieces (size):
    for p in "prnbkqPRNBKQ":
        makePiece (p, size)

#             touchgui.image_gui ("chessimages/%s-white.png" % (name)).white2rgb (.5, .4, .3, .9)]

def chess_black (name):
    # [frozen, active, activated, pressed].
    name = name.upper ()
    # use the white piece and colour it appropriately.
    return [touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (.1, .1, .1, .9),
            touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (.2, .2, .2, .9),
            touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (.4, .4, .4, .9),
            touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (.3, .05, .1, .9)]


def chess_white (name):
    # [frozen, active, activated, pressed].
    return [touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (.5, .5, .5, .9),
            touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (.6, .6, .6, .9),
            touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (1, 1, 1, .9),
            touchgui.image_gui ("chessimages/%s.png" % (name)).white2rgb (.1, .35, .6, .9)]


def blank_square (display, pixels, square):
    x, y = square_coordinate[square]
    if (ord (square[0]) + ord (square[1])) % 2 == 0:
        pygame.draw.rect (display, dark_square, (x, y, pixels, pixels), 0)
    else:
        pygame.draw.rect (display, light_square, (x, y, pixels, pixels), 0)


def blankBoard (display):
    pixels = touchgui.unitY (square_size)
    for y in range (8):
        count = y % 2
        for x in range (8):
            if count % 2 == 0:
                pygame.draw.rect (display, light_square, ((x+1) * pixels, (y+1)*pixels,
                                                          pixels, pixels), 0)
            else:
                pygame.draw.rect (display, dark_square, ((x+1) * pixels, (y+1)*pixels,
                                                         pixels, pixels), 0)
            count += 1
    return display


def event_test (event):
    if (event.type == KEYDOWN) and (event.key == K_ESCAPE):
        myquit (None)


def createBoard (size, board_layout):
    global static_pieces, pieces, square_coordinate
    pixels = touchgui.unitY (size)
    makePieces (pixels)
    static_pieces = []
    pieces = {}
    square_coordinate = {}
    for count, p in enumerate (board_layout):
        x = count % 8
        y = int (count / 8)
        key = "%c%c" % (chr (x + ord ('a')), chr ((7-y) + ord ('1')))
        square_coordinate[key] = [(x+1) * pixels, (y+1) * pixels]
        if p == ".":
            if key in pieces:
                del pieces[key]
        elif p.upper () == p:
            tile = touchgui.image_tile (chess_white (p),
                                        (x+1) * pixels, (y+1) * pixels,
                                        pixels, pixels, square_pressed)
            tile.set_background (None)
            static_pieces += [tile]
            pieces[key] = tile
        else:
            tile = touchgui.image_tile (chess_black (p),
                                        (x+1) * pixels, (y+1) * pixels,
                                        pixels, pixels, square_pressed)
            tile.set_background (None)
            pieces[key] = tile
            static_pieces += [tile]

#
#  sum_distance - return the total number of points which will be plotted by
#                 Bresenham's algorithm should a sequence of lines be drawing
#                 using way_points.
#

def sum_distance (way_points):
    distance = 0
    if len (way_points) > 1:
        curpos = way_points[0]
        for point in way_points[1:]:
            x = abs (curpos[0] - point[0])
            y = abs (curpos[1] - point[1])
            distance += max (x, y)
            curpos = point
    return distance


class movement:
    def __init__ (self, tile, way_points, pos_accel, neg_accel, max_velocity):
        self.tile = tile
        self.way_points_remaining = way_points
        self.pos_accel = pos_accel
        self.neg_accel = neg_accel
        self.distance_remaining = sum_distance (way_points)
        self.max_velocity = max_velocity
        self.velocity = 0
        self.ending = False
        self.bres = None
        self.delay = 0
    def get_tile (self):
        return self.tile
    #
    #  move_pixel - move the tile image one pixel along the way_point route.
    #
    def _move_pixel (self):
        if self.bres == None:
            if self.way_points_remaining == []:
                return
            way = self.way_points_remaining[0]
            if len (self.way_points_remaining) > 1:
                self.way_points_remaining = self.way_points_remaining[1:]
            else:
                self.way_points_remaining = []
            self.bres = bres.bres (self.tile.get_pos (), way)
        self.calculate_velocity ()
        if self.bres.finished ():
            self.bres = None
        else:
            self.distance_remaining = max (self.distance_remaining - 1, 0)
            coord = self.bres.get_next ()
            self.tile.set_pos (coord[0], coord[1])

    #
    #  update the movement of the tile.
    #
    def update (self):
        if self.delay == 0:
            self.delay = max_delay - self.velocity
            self._move_pixel ()
        else:
            self.delay -= 1
    def calculate_velocity (self):
        if self.velocity > 0:
            #
            #  do we need to start decelerating?
            #
            if self.distance_remaining / self.velocity <= self.neg_accel:
                self.ending = True
            #
            #  adjust velocity after applying deceleration
            #
            if self.ending:
                self.velocity = max (0, self.velocity - self.neg_accel)
        #
        #  check to see if we are accelerating
        #
        if (not self.ending) and (self.velocity < self.max_velocity):
            #
            #  adjust velocity after applying acceleration
            #
            self.velocity = min (self.velocity + self.pos_accel, self.max_velocity)
    def finished (self):
        return (self.way_points_remaining == []) and (self.bres == None)


class fade:
    def __init__ (self, tile, start, end, increment, delay = 0):
        self.tile = tile
        self.start = start
        self.end = end
        self.increment = increment
        self.delay = delay
        self.delta_delay = 0
        self.is_finished = False
        self.original_images = tile.get_images ()
        self.pil_image_orig = to_pil (self._get_image ().convert_alpha ())
    def update (self):
        if self.is_finished:
            return
        if self.delta_delay > 0:
            self.delta_delay -= 1
            return
        if self.start == self.end:
            self.is_finished = True
        self._apply_alpha (self.start)
        self.delta_delay = self.delay
        if self.increment > 0:
            self.start = min (self.start + self.increment, self.end)
        else:
            self.start = max (self.start + self.increment, self.end)
    def finished (self):
        return self.is_finished
    #
    #  _set_image - set the active image for the tile to new_image.
    #
    def _set_image (self, new_image):
        # leave the original image list alone and use a copy
        """
        self.tile.set_images ([self.original_images[touchgui.tile_frozen],
                               touchgui.surface_tile (new_image),
                               self.original_images[touchgui.tile_activated],
                               self.original_images[touchgui.tile_pressed]])
        """
        self.tile.set_images ([touchgui.surface_tile (new_image),
                               touchgui.surface_tile (new_image),
                               touchgui.surface_tile (new_image),
                               touchgui.surface_tile (new_image)])

    def _get_image (self):
        return self.original_images[touchgui.tile_active].load_image ()
    def _apply_alpha (self, alpha_value):
        pil_image_rgba = self.pil_image_orig.copy ()
        pil_image_rgba.putalpha (alpha_value)
        gameDisplay.blit (to_pygame (pil_image_rgba), self.tile._image_rect)
    def get_tile (self):
        return self.tile


#
# to_pil - converts an image (surface) from Pygame into PIL format
#

def to_pil (surface):
    raw_string = pygame.image.tostring (surface, "RGBA", False)
    return Image.frombytes ("RGBA", surface.get_size (), raw_string)


#
# to_pygame - converts a PIL image to a to Pygame image (surface).
#

def to_pygame (image):
    return pygame.image.fromstring (image.tobytes (), image.size, "RGBA").convert_alpha ()


def get_moving_pieces ():
    m = []
    for moving in movement_pieces:
        m += [moving.get_tile ()]
    return m


def update_movement ():
    for moving in movement_pieces:
        moving.update ()


#
#  movement_finished - returns True if all movement has completed.
#

def movement_finished ():
    for moving in movement_pieces:
        if not moving.finished ():
            return False
    return True


def create_movement (tile, way_points, pos_accel, neg_accel, max_velocity):
    global movement_pieces
    movement_pieces += [movement (tile, way_points, pos_accel, neg_accel, max_velocity)]


def all_pieces ():
    return static_pieces # + get_moving_pieces ()

#
#  freeze all tiles
#

def freeze_tiles (board_pieces):
    for tile in board_pieces:
        tile.set_frozen ()


#
#  activate all tiles
#

def activate_tiles (tiles):
    for tile in tiles:
        tile.set_active ()


def freeze_unlisted (board_pieces, legal_moves):
    freeze_tiles (board_pieces)
    #
    #  unfreeze pieces which can be moved
    #
    for move in legal_moves:
        if move == "o-o-o":
            pass
        if move == "o-o":
            pass
        if move == "O-O-O":
            pass
        if move == "O-O":
            pass
        key = move[1].lower () + move[2]
        pieces[key].set_active ()
    return board_pieces


#
#  get_dest_squares - return a list of keys to which the selected piece can move.
#

def get_dest_squares (key, legal_moves):
    dest = []
    for move in legal_moves:
        if move == "o-o-o":
            pass
        elif move == "o-o":
            pass
        elif move == "O-O-O":
            pass
        elif move == "O-O":
            pass
        else:
            src_key = move[1].lower () + move[2]
            if src_key == key:
                dest_key = move[4].lower () + move[5]
                dest += [dest_key]
    return dest


#
#  highlight_dest_squares -
#

def highlight_dest_squares (gameDisplay, keylist):
    gameDisplay = blankBoard (gameDisplay)
    pixels = touchgui.unitY (square_size)
    dest = []  #  destination square tiles
    for key in keylist:
        x, y = square_coordinate[key]
        if (ord (key[0]) + ord (key[1])) % 2 == 0:
            dest += [touchgui.image_tile ([touchgui.color_tile (dark_square, pixels, pixels),
                                           touchgui.color_tile (high_dark_square, pixels, pixels),
                                           touchgui.color_tile (high_light_square, pixels, pixels),
                                           touchgui.color_tile (dark_square, pixels, pixels)],
                                          x, y, pixels, pixels, dest_selected)]
        else:
            dest += [touchgui.image_tile ([touchgui.color_tile (light_square, pixels, pixels),
                                           touchgui.color_tile (high_light_square, pixels, pixels),
                                           touchgui.color_tile (high_dark_square, pixels, pixels),
                                           touchgui.color_tile (light_square, pixels, pixels)],
                                          x, y, pixels, pixels, dest_selected)]
    return gameDisplay, dest


#
#  move_combination - moves all pieces in the move_list.  Note it cannot be used
#                     to take a piece or add/replace a piece.
#

def move_combination (move_list):
    global gameDisplay, controls, pieces
    for src, dest in move_list:
        create_movement (pieces[src],
                         [square_coordinate[src], square_coordinate[dest]],
                         1, 1, max_velocity)
    while not movement_finished ():
        gameDisplay = blankBoard (gameDisplay)
        update_movement ()
        is_finished = False
        forms = all_pieces () + controls
        touchgui.select (forms, event_test, finished, 10)
    #
    #  now update the data structures
    #
    for src, dest in move_list:
        pieces[dest] = pieces[src]
        del pieces[src]


def is_match (move, src_key, dest_key):
    if move == src_key:
        # castling short special case
        return True
    print ("move = ", move, src_key, dest_key)
    if len (move) > 4:
        move = move.lower ()
        src_key = src_key.lower ()
        dest_key = dest_key.lower ()
        return (move[1:3] == src_key) and (move[4:6] == dest_key)
    return False


def move_index (legal_moves, move_src_square, move_dest_square):
    for idx, move in enumerate (legal_moves):
        if is_match (move, move_src_square, move_dest_square):
            return idx
    return None


def perform_move (legal_moves, move_src_square, move_dest_square):
    global shell
    idx = move_index (legal_moves, move_src_square, move_dest_square)
    if idx is None:
        print ("perform_move could not implement the move between",
               move_src_square, move_dest_square)
        quit ()
    shell.make_move (idx)
    if move_src_square == "o-o":
        pass
    elif move_src_square == "o-o-o":
        pass
    elif move_src_square == "O-O":
        pass
    elif move_src_square == "O-O-O":
        pass
    else:
        print (move_src_square, move_dest_square)
        move_src_square = move_src_square.lower ()
        move_dest_square = move_dest_square.lower ()
        move_combination ([[move_src_square, move_dest_square]])


def test_fade (position):
    global movement_pieces, pieces, static_pieces
    movement_pieces += [fade (pieces[position], 255, 0, -1)]
    #
    #  now update the data structures
    #
    static_pieces.remove (pieces[position])
    del pieces[position]


def usage (code):
    print ("pychessshell [-d][-h]")
    sys.exit (code)

def process_options ():
    global debugging
    optlist, l = getopt.getopt(sys.argv[1:], ":dh")
    print("optlist =", optlist)
    print("l =", l)
    for opt in optlist:
        if opt[0] == "-h":
            usage (0)
        elif opt[0] == "-d":
            debugging = True
    if l == []:
        return None
    return l[0]


def main ():
    global gameDisplay, controls, shell, is_finished, move_src_square, move_dest_square

    filename = process_options ()
    shell = chessShell.shell (filename)
    board = shell.get_board ()
    pygame.init ()
    if full_screen:
        gameDisplay = pygame.display.set_mode ((display_width, display_height), FULLSCREEN)
    else:
        gameDisplay = pygame.display.set_mode ((display_width, display_height))

    pygame.display.set_caption ("Chess")
    touchgui.set_display (gameDisplay, display_width, display_height)

    gameDisplay.fill (touchguipalate.black)
    gameDisplay = blankBoard (gameDisplay)
    createBoard (square_size, board)
    controls = buttons ()
    legal_moves = shell.get_legal_moves ()
    forms = freeze_unlisted (all_pieces (), legal_moves) + controls
    while True:
        is_finished = False
        touchgui.select (forms, event_test, finished)
        if move_src_square is None:
            print ("move_src_square is None")
            forms = freeze_unlisted (all_pieces (), legal_moves) + controls
        elif move_dest_square is None:
            print ("move_dest_square is None")
            dest_keys = get_dest_squares (move_src_square, legal_moves)
            gameDisplay, dest = highlight_dest_squares (gameDisplay, dest_keys)
            forms = dest + freeze_unlisted (all_pieces (), legal_moves) + controls
            pieces[move_src_square].set_frozen ()       #  and freeze the chosen piece
        else:
            print ("reached completed move")
            # perform the move
            print ("values =", move_src_square, move_dest_square)
            perform_move (legal_moves, move_src_square, move_dest_square)
            move_src_square = None
            move_dest_square = None
            gameDisplay = blankBoard (gameDisplay)  # redraw the board to remove highlighted squares
            legal_moves = shell.get_legal_moves ()
            forms = freeze_unlisted (all_pieces (), legal_moves) + controls



"""
    quit ()

    #
    #
    #
    # blank_square (gameDisplay, touchgui.unitY (0.1), "h1")
    test_fade ("h1")
    move_combination ([["e2", "e4"]])
    move_combination ([["b1", "c3"]])
    move_combination ([["d2", "d4"]])
    move_combination ([["c1", "g5"]])
    move_combination ([["d1", "b1"], ["a1", "c1"]])
    move_combination ([["d7", "d5"]])
    test_fade ("d5")
    move_combination ([["e4", "d5"]])
    forms = all_pieces () + controls
"""


main ()
