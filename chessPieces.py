#!/usr/bin/env python3

import chess, os, chess.svg

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


def myquit (name = None, tap = None):
    print("quit called")
    pygame.display.update ()  # need this to see the button pressed before we quit
    pygame.time.delay (toggle_delay * 2) #  delay program so we see the button change
    pygame.quit ()  #  now shutdown pygame
    quit ()  #  and shutdown python


def pressed (name, tap = None):
    print((name, "pressed"))


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


def makeBoard (size, board):
    pieces = []
    pixels = touchgui.unitY (size)
    makePieces (pixels)
    for count, p in enumerate (board):
        x = count % 8
        y = int (count / 8)
        if p == ".":
            pass
        elif p.upper () == p:
            t = touchgui.image_tile (chess_white (p),
                                     (x+1) * pixels, (y+1) * pixels,
                                     pixels, pixels, pressed)

        else:
            t = touchgui.image_tile (chess_black (p),
                                     (x+1) * pixels, (y+1) * pixels,
                                     pixels, pixels, pressed)
        t.set_background (None)
        pieces += [t]
    return pieces


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

def main ():
    pygame.init ()
    if full_screen:
        gameDisplay = pygame.display.set_mode ((display_width, display_height), FULLSCREEN)
    else:
        gameDisplay = pygame.display.set_mode ((display_width, display_height))

    pygame.display.set_caption ("Chess")
    touchgui.set_display (gameDisplay, display_width, display_height)

    gameDisplay.fill (touchguipalate.black)
    gameDisplay = blankBoard (gameDisplay, 0.1)
    forms = makeBoard (0.1, "rnbkqbnrpppppppp................................PPPPPPPPRNBKQBNR") + buttons ()
    touchgui.select (forms, event_test)

main ()
