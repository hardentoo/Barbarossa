{-# LANGUAGE BangPatterns #-}
module Eval.BasicEval (
    matPiece, gradedMatVal
) where

import Data.Array.Unboxed
import Data.Array.Base
import GHC.Arr (unsafeIndex)
import Data.Bits (unsafeShiftR)

import Struct.Struct

pawnVal, kingVal :: Int
pawnVal = 100
kingVal = 20000

matvals :: UArray Piece Int
matvals = listArray (Pawn, King) [ pawnVal, 325, 325, 500, 975, kingVal ]

{-# INLINE matPiece #-}
matPiece :: Color -> Piece -> Int
matPiece White = unsafeAt matvals . unsafeIndex (Pawn, King)
matPiece Black = negate . unsafeAt matvals . unsafeIndex (Pawn, King)

-- The pawns are graded by the stage
-- The pieces are graded based on number of pawns
gradedMatVal :: Piece -> Int -> Int
gradedMatVal King   !i = error "King graded??"
gradedMatVal Queen  !i = unsafeAt  queenGrades i
gradedMatVal Rook   !i = unsafeAt   rookGrades i
gradedMatVal Bishop !i = unsafeAt bishopGrades i
gradedMatVal Knight !i = unsafeAt knightGrades i
-- Tricky here: we know is must be in centipawns, but calculate in 8th of that
-- to be more precise. The pawn value is included in the function! (Now 100)
gradedMatVal Pawn   !s = case v > 115 of
                             True -> 115
                             _    -> case v < 100 of
                                         True -> 100
                                         _    -> v
    where !v = (24500 - s) `div` 200

queenGrades, rookGrades, bishopGrades, knightGrades :: UArray Int Int
-- These arrays are used to grade the pieces and pawns
-- The values depend on the values of the pieces (array matvals)
-- So when those are changes, these arrays should be recalculated
-- The classification of positions go from open (0-4 pawns), to semi-open
-- (5-8 pawns), semi-closed (9-12 pawns) and closed (13-16 pawns)
-- The value of pieces is adjusted according with this classification
-- Some pieces lose value for closed, other for opened positions
-- Queens:
queenGrades = listArray (0, 16) [ 1268, 1268, 1268, 1266, 1242, 1218, 1194, 1170, 1109,
                                        1048,  987,  926,  914,  902,  890,  878,  878 ]
-- Rooks:
rookGrades  = listArray (0, 16) [  550,  550,  550,  550,  550,  550,  550,  525,  500,
                                         470,  450,  450,  450,  450,  450,  450,  450 ]
-- Bishops:
bishopGrades = listArray (0, 16) [ 390,  390,  390,  390,  385,  379,  374,  374,  366,
                                         355,  341,  341,  335,  330,  325,  325,  325 ]
-- Knights:
knightGrades = listArray (0, 16) [ 276,  276,  276,  276,  281,  287,  293,  293,  293,
                                         314,  335,  356,  356,  356,  356,  356,  356 ]
