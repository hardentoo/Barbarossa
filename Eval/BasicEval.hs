module Eval.BasicEval (
    psqOn,
    -- psqRm, psqFromTo,
    gamePhaseVal, matPiece
) where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, fromList, backpermute, (!))

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
pslKingM = zipWith (+) (repeat (negate rest))
           [ rest+15, rest+50, rest-20, rest-30, rest-30, rest-20, rest+50, rest+15,
             rest+0,  rest+0,  rest-10, rest-30, rest-30, rest-10, rest+0, rest+0,
               0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0 ]
    where rest = 325
pslKingE = zipWith (+) (map mvals pslKingM)
           [ 0,  0,  0,  0,  0,  0,  0, 0,
             0, 10, 10, 10, 10, 10, 10, 0,
             0, 10, 30, 30, 30, 30, 10, 0,
             0, 10, 30, 60, 60, 30, 10, 0,
             0, 10, 30, 60, 60, 30, 10, 0,
             0, 10, 30, 30, 30, 30, 10, 0,
             0, 10, 10, 10, 10, 10, 10, 0,
             0,  0,  0,  0,  0,  0,  0, 0 ]
    where mvals x = round $ negate $ fact * fromIntegral x
          fact = 0.5
-- Pawns:
pslPawnM = [   0,   0,   0,   0,   0,   0,   0,   0,
             100, 110, 110,  85,  85, 110, 110, 110,
             100, 100, 100,  95,  95, 100, 100, 100,
              90, 100, 110, 125, 125, 110, 100,  90,
              90, 100, 100, 125, 125, 100, 100,  90,
             115, 120, 120, 125, 125, 120, 120, 115,
              95, 100, 100, 105, 105, 100, 100,  95,
               0,   0,   0,   0,   0,   0,   0,   0 ]
pslPawnE = [   0,   0,   0,   0,   0,   0,   0,   0,
              80,  80,  80,  65,  65,  80,  80,  80,
              85, 110,  90,  65,  65, 100, 100,  85,
              90, 120, 120, 125, 125, 120, 120,  90,
              95, 125, 125, 125, 125, 125, 125,  95,
             110, 120, 125, 125, 125, 120, 120, 110,
             125, 145, 145, 145, 145, 145, 145, 125,
               0,   0,   0,   0,   0,   0,   0,   0 ]
-- Knights:
pslKnightM = [ 270, 285, 295, 295, 295, 295, 285, 270,
               285, 295, 305, 305, 305, 305, 295, 285,
               295, 305, 330, 325, 325, 330, 305, 295,
               295, 270, 325, 325, 325, 325, 270, 295,
               295, 270, 330, 330, 330, 330, 270, 295,
               270, 270, 305, 305, 305, 305, 270, 270,
               285, 270, 270, 270, 270, 270, 270, 285,
               270, 270, 270, 270, 270, 270, 270, 270 ]
pslKnightE = [ 270, 285, 295, 295, 295, 295, 285, 270,
               285, 295, 305, 305, 305, 305, 295, 285,
               295, 305, 320, 325, 325, 320, 305, 295,
               295, 305, 325, 325, 325, 325, 305, 295,
               295, 305, 325, 325, 325, 325, 305, 295,
               295, 305, 320, 325, 325, 320, 305, 295,
               285, 295, 305, 305, 305, 305, 295, 285,
               270, 285, 295, 295, 295, 295, 285, 270 ]

psaKingMW, psaKingEW, psaKingMB, psaKingEB,
    psaPawnMW, psaPawnEW, psaPawnMB, psaPawnEB,
    psaKnightMW, psaKnightEW, psaKnightMB, psaKnightEB :: Vector Int

mirror :: Vector Int -> Vector Int
mirror v = V.map negate $ backpermute v mirrorx
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
