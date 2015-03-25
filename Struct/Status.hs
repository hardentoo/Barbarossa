module Struct.Status (
    Stats(..),
    MyState(..),
    EvalState(..),
    EvalParams(..),
    Explo(..)
) where

import Data.Array.IArray

import Struct.Struct
import Moves.History
import Hash.TransTab
import Hash.Queue

data Stats = Stats {
        nodes :: !Int,
        maxmvs :: !Int
    } deriving Show

data Explo = Explo {
        exPos :: MyPos,
        exPly, exAlpha, exBeta :: !Int,
        exPAlpha, exPBeta, exPRet :: [Move]
    }

data MyState = MyState {
        stack :: [MyPos],	-- stack of played positions
        hash  :: Cache,		-- transposition table
        hist  :: History,	-- history table
        stats :: !Stats,	-- statistics
        evalst :: EvalState,	-- eval status (parameter & statistics)
        explo :: Array Int (Queue Explo)	-- keep information abt search explosion
    }

data EvalState = EvalState {
        esDWeightsM :: [Double],
        esDWeightsE :: [Double],
        esIWeightsM :: [Int],
        esIWeightsE :: [Int],
        esEParams   :: EvalParams
    } deriving Show

-- This is the parameter record for characteristics evaluation
data EvalParams
    = EvalParams {
          -- Parameters of the king placement
          epMovingMid  :: !Int,
          epMovingEnd  :: !Int,
          epMaterMinor :: !Int,
          epMaterRook  :: !Int,
          epMaterQueen :: !Int,
          epMaterScale :: !Int,
          epMaterBonusScale :: !Int,
          epPawnBonusScale  :: !Int,
          epPassKingProx    :: !Int,
          epPassBlockO :: !Int,
          epPassBlockA :: !Int,
          epPassMin    :: !Int,
          epPassMyCtrl :: !Int,
          epPassYoCtrl :: !Int
      } deriving Show
