module Eval.BasicEval (
    psqOn,
    -- psqRm, psqFromTo,
    gamePhaseVal, matPiece
) where

import Prelude hiding (map)
import Data.Vector.Unboxed (Vector, fromList, backpermute, (!), map)

import Struct.Struct

matPiece1 :: Piece -> Int
matPiece1 Pawn   = 100
matPiece1 Knight = 325
matPiece1 Bishop = 325
matPiece1 Rook   = 500
matPiece1 Queen  = 975
matPiece1 King   = 20000

fun :: Color -> Int -> Int
fun White = id
fun Black = negate

{-# INLINE matPiece #-}
matPiece :: Color -> Piece -> Int
matPiece c = fun c . matPiece1

{-# INLINE gamePhaseVal #-}
gamePhaseVal :: Piece -> Int
gamePhaseVal Queen  = 39
gamePhaseVal Rook   = 20
gamePhaseVal Bishop = 12
gamePhaseVal Knight = 12
gamePhaseVal _      =  0

{--
{-# INLINE psqFromTo #-}
psqFromTo :: Color -> Piece -> Square -> Square -> (Int, Int)
psqFromTo White King f t = ( psaKingMW ! t - psaKingMW ! f,
                             psaKingEW ! t - psaKingEW ! f )
psqFromTo Black King f t = ( psaKingMB ! t - psaKingMB ! f,
                             psaKingEB ! t - psaKingEB ! f )
psqFromTo White Pawn f t = ( psaPawnMW ! t - psaPawnMW ! f,
                             psaPawnEW ! t - psaPawnEW ! f )
psqFromTo Black Pawn f t = ( psaPawnMB ! t - psaPawnMB ! f,
                             psaPawnEB ! t - psaPawnEB ! f )
psqFromTo White Knight f t = ( psaKnightMW ! t - psaKnightMW ! f,
                               psaKnightEW ! t - psaKnightEW ! f )
psqFromTo Black Knight f t = ( psaKnightMB ! t - psaKnightMB ! f,
                               psaKnightEB ! t - psaKnightEB ! f )
psqFromTo _ _ _ _ = (0, 0)

{-# INLINE psqRm #-}
psqRm :: Color -> Piece -> Square -> (Int, Int)
psqRm c p s = negpair $ psqOn c p s
    where negpair (a, b) = (-a, -b)
--}

{-# INLINE psqOn #-}
psqOn :: Color -> Piece -> Square -> (Int, Int)
psqOn White King s = (psaKingMW ! s, psaKingEW ! s)
psqOn Black King s = (psaKingMB ! s, psaKingEB ! s)
psqOn White Pawn s = (psaPawnMW ! s, psaPawnEW ! s)
psqOn Black Pawn s = (psaPawnMB ! s, psaPawnEB ! s)
psqOn White Knight s = (psaKnightMW ! s, psaKnightEW ! s)
psqOn Black Knight s = (psaKnightMB ! s, psaKnightEB ! s)
psqOn White p _ = let v = matPiece1 p in (v, v) 
psqOn Black p _ = let v = negate (matPiece1 p) in (v, v)

-- These are the piece square lists from white pov
-- which are used to create the piece square vectors
-- The position is the square number (0 = A1, 63 = H8)
-- The values are in centi pawns and contain the
-- piece material value for the game phase (mid game, end game)
pslKingM, pslKingE, pslPawnM, pslPawnE, pslKnightM, pslKnightE :: [Int]
--            A1
pslKingM = [  25,  100,   50,    0,    0,    0,  125,   50,
              20,   10,    0,  -30,  -30,   10,    0,   20,
            -100, -125, -150, -175, -175, -150, -125, -100,
            -200, -225, -250, -275, -275, -250, -225, -200,
            -300, -325, -350, -375, -375, -350, -325, -300,
            -400, -425, -450, -475, -475, -450, -425, -400,
            -500, -500, -500, -500, -500, -500, -500, -500,
            -500, -500, -500, -500, -500, -500, -500, -500 ]
pslKingE = [   0,  0,  0,  0,  0,  0,  0,  0,
               0, 10, 10, 10, 10, 10, 10,  0,
              10, 20, 30, 40, 40, 30, 20, 10,
              15, 30, 40, 60, 60, 40, 30, 15,
              15, 30, 40, 60, 60, 40, 30, 15,
              10, 20, 30, 40, 40, 30, 20, 10,
               0, 10, 10, 10, 10, 10, 10,  0,
               0,  0,  0,  0,  0,  0,  0,  0 ]
pslPawnM = [   0,   0,   0,   0,   0,   0,   0,   0,
             100, 100, 100,  75,  75, 100, 100, 100,
             100, 100,  90,  65,  65,  90, 100, 100,
              90,  60, 110, 125, 125, 110, 100,  90,
              90,  70, 100, 125, 125, 100, 100,  85,
             115, 120, 120, 125, 125, 120, 120, 115,
              95, 100, 100, 105, 105, 100, 100,  95,
               0,   0,   0,   0,   0,   0,   0,   0 ]
pslPawnE = [   0,   0,   0,   0,   0,   0,   0,   0,
              80,  80,  80,  65,  65,  80,  80,  80,
              85, 110,  90,  65,  65, 100, 100,  85,
              90, 120, 120, 125, 125, 120, 120,  90,
              95, 125, 125, 125, 125, 125, 125,  95,
             115, 125, 130, 135, 135, 130, 125, 115,
             130, 140, 150, 155, 155, 150, 150, 130,
               0,   0,   0,   0,   0,   0,   0,   0 ]
pslKnightM = [ 200, 205, 210, 210, 210, 210, 205, 200,
               205, 210, 225, 225, 225, 225, 210, 205,
               210, 280, 330, 300, 300, 330, 280, 210,
               205, 270, 325, 325, 325, 325, 270, 205,
               200, 270, 325, 325, 325, 325, 270, 200,
               200, 250, 305, 305, 305, 305, 250, 200,
               200, 240, 250, 250, 250, 250, 240, 200,
               150, 160, 170, 170, 170, 170, 160, 150 ]
pslKnightE = [ 200, 205, 210, 210, 210, 210, 205, 200,
               205, 210, 225, 225, 225, 225, 210, 205,
               210, 280, 305, 300, 300, 305, 280, 210,
               220, 270, 325, 325, 325, 325, 270, 220,
               220, 270, 325, 325, 325, 325, 270, 220,
               210, 250, 305, 305, 305, 305, 250, 210,
               205, 210, 225, 225, 225, 225, 210, 205,
               200, 205, 210, 210, 210, 210, 205, 200 ]

psaKingMW, psaKingEW, psaKingMB, psaKingEB,
    psaPawnMW, psaPawnEW, psaPawnMB, psaPawnEB,
    psaKnightMW, psaKnightEW, psaKnightMB, psaKnightEB :: Vector Int

mirror :: Vector Int -> Vector Int
mirror v = map negate $ backpermute v mirrorx
    where mirrorx :: Vector Int
          mirrorx = fromList $ [ 56 .. 63 ] ++ [ 48 .. 55 ] ++ [ 40 .. 47 ] ++ [ 32 .. 39 ]
                            ++ [ 24 .. 31 ] ++ [ 16 .. 23 ] ++ [  8 .. 15 ] ++ [  0 ..  7 ]

psaKingMW = fromList pslKingM
psaKingEW = fromList pslKingE
psaKingMB = mirror psaKingMW
psaKingEB = mirror psaKingEW
psaPawnMW = fromList pslPawnM
psaPawnEW = fromList pslPawnE
psaPawnMB = mirror psaPawnMW
psaPawnEB = mirror psaPawnEW
psaKnightMW = fromList pslKnightM
psaKnightEW = fromList pslKnightE
psaKnightMB = mirror psaKnightMW
psaKnightEB = mirror psaKnightEW
