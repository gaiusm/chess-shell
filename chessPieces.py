#!/usr/bin/env python3

import chess, os, chess.svg, bres

import pygame, touchgui, touchguipalate, touchguiconf
from touchgui import posX, posY
from pygame.locals import *

# display_width, display_height = 1920, 1080
display_width, display_height = 800, 600
display_width, display_height = 1920, 1080
full_screen = False
full_screen = True
toggle_delay = 250
light_square = (166, 124, 54)
dark_square  = (76, 47, 0)
movement_pieces = []
is_finished = False
max_delay = 10
max_velocity = max_delay


def myquit (name = None, tap = None):
    print ("quit called")
    pygame.display.update ()  # need this to see the button pressed before we quit
    pygame.time.delay (toggle_delay * 2) #  delay program so we see the button change
    pygame.quit ()  #  now shutdown pygame
    quit ()  #  and shutdown python


def pressed (name, tap = None):
    global is_finished
    is_finished = True
    print (name, "pressed")


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


def blankBoard (display, size):
    pixels = touchgui.unitY (size)
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
                                        pixels, pixels, pressed)
            tile.set_background (None)
            static_pieces += [tile]
            pieces[key] = tile
        else:
            tile = touchgui.image_tile (chess_black (p),
                                        (x+1) * pixels, (y+1) * pixels,
                                        pixels, pixels, pressed)
            tile.set_background (None)
            pieces[key] = tile
            static_pieces += [tile]

#
#
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
    def move_pixel (self):
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
            self.move_pixel ()
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
    return static_pieces + get_moving_pieces ()


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
        gameDisplay = blankBoard (gameDisplay, 0.1)
        update_movement ()
        is_finished = False
        forms = all_pieces () + controls
        touchgui.select (forms, event_test, finished, 1)
    #
    #  now update the data structures
    #
    for src, dest in move_list:
        pieces[dest] = pieces[src]
        del pieces[src]


def main ():
    global gameDisplay, controls
    pygame.init ()
    if full_screen:
        gameDisplay = pygame.display.set_mode ((display_width, display_height), FULLSCREEN)
    else:
        gameDisplay = pygame.display.set_mode ((display_width, display_height))

    pygame.display.set_caption ("Chess")
    touchgui.set_display (gameDisplay, display_width, display_height)

    gameDisplay.fill (touchguipalate.black)
    gameDisplay = blankBoard (gameDisplay, 0.1)
    createBoard (0.1, "rnbkqbnrpppppppp................................PPPPPPPPRNBKQBNR")
    controls = buttons ()
    forms = all_pieces () + controls
    touchgui.select (forms, event_test, finished)
    #
    #
    #
    move_combination ([["e2", "e4"]])
    move_combination ([["b1", "c3"]])
    move_combination ([["d2", "d4"]])
    move_combination ([["c1", "g5"]])
    move_combination ([["d1", "b1"], ["a1", "c1"]])
    move_combination ([["d7", "d5"]])
    move_combination ([["e4", "d5"]])
    touchgui.select (forms, event_test)


main ()
