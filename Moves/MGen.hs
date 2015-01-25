{-# LANGUAGE BangPatterns #-}

module Moves.MGen (
    MGen,
    makePureGen, makeGameGen, makeFiltGen, makeListGen,
    getNextMove
) where

import Data.List
-- import Control.Monad.State
-- import Control.Monad.Reader (ask)
-- import Data.Ord (comparing)
-- import Numeric
-- import System.Random

import Moves.BaseTypes
import Struct.Struct

data MGen = MGPure [Move]
          | MGGame (Game [Move])
          | MGFilt [Move] MGen
          | MGGens [MGen]

makePureGen :: [Move] -> MGen
makePureGen = MGPure

makeGameGen :: (Game [Move]) -> MGen
makeGameGen = MGGame

makeFiltGen :: [Move] -> MGen -> MGen
makeFiltGen = MGFilt

makeListGen :: [MGen] -> MGen
makeListGen = MGGens

getNextMove :: MGen -> Game (Maybe (Move, MGen))
getNextMove (MGPure [])     = return Nothing
getNextMove (MGPure (m:ms)) = return $ Just (m, MGPure ms)
getNextMove (MGGame act)    = do
    mvs <- act
    case mvs of
        []   -> return Nothing
        m:ms -> return $ Just (m, MGPure ms)
getNextMove (MGFilt [] g) = getNextMove g
getNextMove (MGFilt ms g) = do
    mr <- getNextMove g
    case mr of
        Nothing      -> return Nothing
        Just (m, g') -> if m `elem` ms
                           then getNextMove (MGFilt (delete m ms) g')
                           else return $ Just (m, MGFilt ms g')
getNextMove (MGGens [])     = return Nothing
getNextMove (MGGens (g:gs)) = do
    mr <- getNextMove g
    case mr of
        Nothing      -> getNextMove (MGGens gs)
        Just (m, g') -> return $ Just (m, MGGens (g':gs))
