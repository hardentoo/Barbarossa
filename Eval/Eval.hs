{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.Eval (
    initEvalState,
    posEval,
    maxStatsDepth, maxStatsIntvs,
    inLimits,
    weightNames, weightLims, parDim, weightPairs
) where

-- This branch has the evaluation function mostly inspired by the paper
-- Little Chess Evaluation Compendium by Tsevtkov
-- Most of the features from that paper are not yet implemented, this is a work in progress...

import Prelude hiding ((++), head, foldl, map, concat, filter, takeWhile, iterate, sum, minimum,
                       zip, zipWith, foldr, concatMap, length, replicate, lookup, repeat, null)
import Data.Array.Base (unsafeAt)
import Data.Bits hiding (popCount)
import Data.List.Stream
import Control.Monad.State.Lazy
import Data.Array.Unboxed
import Data.Ord (comparing)

import Struct.Struct
import Struct.Status
import Struct.Config
import Moves.Moves
import Moves.BitBoard
import Moves.Muster
import Eval.BasicEval

-- These are characteristics weights
type IWeights = [Int]
type DWeights = [Double]
type Limits = [(Double, Double)]

instance CollectParams EvalParams where
    type CollectFor EvalParams = EvalParams
    npColInit = EvalParams {
                    epMaterMinor = 1,
                    epMaterRook  = 3,
                    epMaterQueen = 11,
                    epMaterScale = 1,
                    epMaterBonusScale = 4,
                    epPawnBonusScale  = 4
                }
    npColParm = collectEvalParams
    npSetParm = id

collectEvalParams :: (String, Double) -> EvalParams -> EvalParams
collectEvalParams (s, v) ep = lookApply s v ep [
        ("epMaterMinor",      setEpMaterMinor),
        ("epMaterRook",       setEpMaterRook),
        ("epMaterQueen",      setEpMaterQueen),
        ("epMaterScale",      setEpMaterScale),
        ("epMaterBonusScale", setEpMaterBonusScale),
        ("epPawnBonusScale",  setEpPawnBonusScale)
    ]
    where setEpMaterMinor      v ep = ep { epMaterMinor      = round v }
          setEpMaterRook       v ep = ep { epMaterRook       = round v }
          setEpMaterQueen      v ep = ep { epMaterQueen      = round v }
          setEpMaterScale      v ep = ep { epMaterScale      = round v }
          setEpMaterBonusScale v ep = ep { epMaterBonusScale = round v }
          setEpPawnBonusScale  v ep = ep { epPawnBonusScale  = round v }

class EvalItem a where
    evalItem    :: EvalParams -> MyPos -> a -> IWeights
    evalItemNDL :: a -> [(String, (Double, (Double, Double)))]	-- Name, Default, Limits

-- some handy functions for eval item types:
weightName :: (a, b) -> a
weightName    = fst

weightDefault :: (a, (b, c)) -> b
weightDefault = fst . snd

weightLimits :: (a, (b, c)) -> c
weightLimits  = snd . snd

data AnyEvalItem = forall a . EvalItem a => EvIt a

-- This is the list of evaluated characteristics of a positions
-- Every item can have one or more parameters which have a name, a default value
-- and a range of values (values are kept for learning purposes as doubles,
-- but for the evaluation itself one copy of integer parameter values is also kept)
-- Item Material must be first in the list, as the material must be known in other
-- places too, so it is picked up from the fist place
evalItems :: [AnyEvalItem]
evalItems = [ EvIt Material,	-- material balance (i.e. white - black material)
              EvIt EnPrise,	-- when not quiescent - pieces en prise
              EvIt Defend,	-- defended and undefended pieces
              EvIt Redundance,	-- bishop pair and rook redundance
              EvIt RookPawn,	-- the rook pawns are about 15% less valuable
              EvIt KingSafe,	-- king safety
              EvIt KingOpen,	-- malus for king openness
              -- EvIt KingCenter,	-- malus for king on center files
              EvIt KingPlace,	-- bonus when king is near some fields
              -- EvIt KingMob,	-- bonus for restricted mobility of adverse king when alone
              -- EvIt Castles,	-- bonus for castle rights
              -- EvIt LastLine,	-- malus for pieces on last line (except rooks and king)
              EvIt Mobility,	-- pieces mobility
              EvIt Center,	-- attacs of center squares
              -- EvIt DblPawns,	-- malus for doubled pawns
              EvIt PassPawns	-- pass pawns
            ]

parDim :: Int
parDim = sum $ map evalLen evalItems
    where evalLen (EvIt a) = length $ evalItemNDL a

weightLims :: [(Double, Double)]
weightLims = concatMap evalLims evalItems
    where evalLims (EvIt a) = map weightLimits $ evalItemNDL a

-- zeroParam :: DWeights
-- zeroParam = replicate parDim 0	-- theese are doubles

zeroFeats :: [Int]
zeroFeats = replicate parDim 0	-- theese are ints

evalItemPar :: EvalItem a => a -> DWeights -> (String, Double) -> Maybe DWeights
evalItemPar a dps (s, v) = lookup s (zip lu posi) >>= \i -> Just (replace dps i v)
    where lu = map weightName $ evalItemNDL a
          replace []       _ _ = []
          replace (_ : ds) 0 v' = v' : ds
          replace (d : ds) i v' = d : replace ds (i-1) v'
          posi = [0..] :: [Int]

oneWeight :: [(AnyEvalItem, DWeights)] -> (String, Double) -> [(AnyEvalItem, DWeights)]
oneWeight [] _ = []
oneWeight (evp@(EvIt ei, dp) : evps) sd
    | Just ndp <- evalItemPar ei dp sd = (EvIt ei, ndp) : evps
    | otherwise = evp : oneWeight evps sd

-- Map a list of parameter assignments (name, value)
-- to a vector of parameter, taking defaults for missing parameters
allWeights :: [(String, Double)] -> DWeights
allWeights = concatMap snd . foldl oneWeight defevps
    where defevps = map defp evalItems
          defp ei@(EvIt a) = (ei, map weightDefault $ evalItemNDL a)

weightNames :: [String]
weightNames = concatMap pnames evalItems
    where pnames (EvIt a) = map weightName $ evalItemNDL a

weightPairs :: [Int] -> [(String, Int)]
weightPairs = zip weightNames

------------------------------------------------------------------
-- Parameters of this module ------------
granCoarse, granCoarse2, granCoarseM, maxStatsDepth, maxStatsIntvs, shift2Cp :: Int
granCoarse    = 4	-- coarse granularity
granCoarse2   = granCoarse `div` 2
granCoarseM   = complement (granCoarse - 1)
shift2Cp      = 3	-- we have 2^shift2Cp units per centipawn
maxStatsDepth = 12	-- for error statistics of the eval function - maximum depth
maxStatsIntvs = 20	-- number of difference interval
-----------------------------------------------

initEvalState :: [(String, Double)] -> EvalState
initEvalState sds = EvalState {
        esDWeights = weights,
        esIWeights = map round weights,
        esEParams  = npSetParm (colParams sds :: CollectFor EvalParams)
    }
    where weights = inLimits weightLims $ allWeights sds

inLimits :: Limits -> DWeights -> DWeights
inLimits ls ps = map inlim $ zip ls ps
    where inlim ((mi, ma), p) = max mi $ min ma p

(<*>) :: Num a => [a] -> [a] -> a
a <*> b = sum $ zipWith (*) a b
{-# SPECIALIZE (<*>) :: [Int] -> [Int] -> Int #-}

matesc :: Int
matesc = 20000 - 255	-- attention, this is also defined in Base.hs!!

posEval :: MyPos -> State EvalState (Int, Int, [Int])
posEval !p = do
    sti <- get
    let (sce, feat) = evalDispatch p sti
        !scl = min matesc $ max (-matesc) sce
        !scc = if granCoarse > 0 then (scl + granCoarse2) .&. granCoarseM else scl
        !matv = head feat
    return (scc, matv, feat)

evalDispatch :: MyPos -> EvalState -> (Int, [Int])
evalDispatch p sti
    | pawns p == 0 = evalNoPawns p sti
    | pawns p .&. me p == 0 ||
      pawns p .&. yo p == 0 = evalSideNoPawns p sti
    | kings p .|. pawns p == occup p,
      Just r <- pawnEndGame p = r
    | otherwise    = normalEval p sti

itemEval :: EvalParams -> MyPos -> AnyEvalItem -> [Int]
itemEval ep p (EvIt a) = evalItem ep p a

normalEval :: MyPos -> EvalState -> (Int, [Int])
normalEval p sti = (sc, feat)
    where !feat = concatMap (itemEval (esEParams sti) p) evalItems
          !sc'  = feat <*> esIWeights sti `shiftR` shift2Cp
          !sc   = sc' + meMoving
          meMoving = 5	-- advantage for moving

evalSideNoPawns :: MyPos -> EvalState -> (Int, [Int])
evalSideNoPawns p sti
    | npwin && insufficient = (0, zeroFeats)
    | otherwise             = (nsc, feats)
    where (nsc, feats) = normalEval p sti
          npside = if pawns p .&. me p == 0 then me p else yo p
          npwin = npside == me p && nsc > 0 || npside == yo p && nsc < 0
          insufficient = majorcnt == 0 && (minorcnt == 1 || minorcnt == 2 && bishopcnt == 0)
          bishopcnt = popCount1 $ bishops p .&. npside
          minorcnt  = popCount1 $ (bishops p .|. knights p) .&. npside
          majorcnt  = popCount1 $ (queens p .|. rooks p) .&. npside

-- These evaluation function distiguishes between some known finals with no pawns
evalNoPawns :: MyPos -> EvalState -> (Int, [Int])
evalNoPawns p sti = (sc, zeroFeats)
    where !sc | onlykings   = 0
              | kmk || knnk = 0		-- one minor or two knights
              | kbbk        = mateKBBK p kaloneyo	-- 2 bishops
              | kbnk        = mateKBNK p kaloneyo	-- bishop + knight
              | kMxk        = mateKMajxK p kaloneyo	-- simple mate with at least one major
              -- | kqkx        = mateQRest p kaloneb	-- queen against minor or rook
              | otherwise   = fst $ normalEval p sti
          kaloneme = me p `less` kings p == 0
          kaloneyo = yo p `less` kings p == 0
          onlykings = kaloneme && kaloneyo
          kmk  = (kaloneme || kaloneyo) && minorcnt == 1 && majorcnt == 0
          knnk = (kaloneme || kaloneyo) && minorcnt == 2 && majorcnt == 0 && bishops p == 0
          kbbk = (kaloneme || kaloneyo) && minorcnt == 2 && majorcnt == 0 && knights p == 0
          kbnk = (kaloneme || kaloneyo) && minorcnt == 2 && not (knnk || kbbk)
          kMxk = (kaloneme || kaloneyo) && majorcnt > 0
          minor   = bishops p .|. knights p
          minorcnt = popCount1 minor
          major    = queens p .|. rooks p
          majorcnt = popCount1 major

winBonus :: Int
winBonus = 200	-- when it's known win

mateKBBK :: MyPos -> Bool -> Int
mateKBBK = mateScore centerDistance

-- It seems that with 2 bishops or 1 major it's the same
-- rule to go to mate
mateKMajxK :: MyPos -> Bool -> Int
mateKMajxK = mateKBBK

mateKBNK :: MyPos -> Bool -> Int
mateKBNK p = mateScore (bnMateDistance wbish) p
    where wbish = bishops p .&. lightSquares /= 0

{-# INLINE mateScore #-}
mateScore :: (Square -> Int) -> MyPos -> Bool -> Int
mateScore f p mywin = msc
    where !kadv = if mywin then ky else km
          !km = kingSquare (kings p) (me p)
          !ky = kingSquare (kings p) (yo p)
          !distk = squareDistance km ky
          !distc = f kadv
          !sc = winBonus + distc*distc - distk*distk
          !mtr = if moving p == White then mater p else -(mater p)
          !wsc = if mywin then sc else -sc
          !msc = mtr + wsc

squareDistArr :: UArray (Square, Square) Int
squareDistArr = array ((0,0), (63,63)) [((s1, s2), squareDist s1 s2) | s1 <- [0..63], s2 <- [0..63]]
    where squareDist f t = max (abs (fr - tr)) (abs (fc - tc))
              where (fr, fc) = f `divMod` 8
                    (tr, tc) = t `divMod` 8

squareDistance :: Square -> Square -> Int
squareDistance = curry (squareDistArr!)

-- This center distance should be pre calculated
centerDistance :: Int -> Int
centerDistance sq = max (r - 4) (3 - r) + max (c - 4) (3 - c)
    where (r, c) = sq `divMod` 8

-- This distance for knight bishop mate should be pre calculated
-- Here we have to push the adverse king far from center and from the opposite bishop corners
bnMateDistance :: Bool -> Square -> Int
bnMateDistance wbish sq = min (squareDistance sq ocor1) (squareDistance sq ocor2)
    where (ocor1, ocor2) = if wbish then (0, 63) else (7, 56)

-- Some helper functions:

{-# INLINE zoneAttacs #-}
zoneAttacs :: MyPos -> BBoard -> (Int, Int)
zoneAttacs p zone = (m, y)
    where m = popCount $ zone .&. myAttacs p
          y = popCount $ zone .&. yoAttacs p

-- In order to consider the game stage, we need to interpolate
-- between some beginning value and some ending value
-- We solve this generically by the following modell:
-- The interpolation is linear, but with limits
-- We define an x domain between xmin and xmax, where the value is linear
-- but under xmin and over xmax the values are fixed: ymin and ymax 
stageInterp :: (Int, Int) -> (Int, Int) -> Int -> Int
stageInterp (xmin, ymin) (xmax, ymax) x
    | x <= xmin = ymin
    | x >= xmax = ymax
    | otherwise = ymin + ydiff * (x - xmin) `div` xdiff
    where ydiff = ymax - ymin
          xdiff = xmax - xmin

----------------------------------------------------------------------------
-- Here we have the implementation of the evaluation items
-- They do not return a score, but a vector of fulfillments of some criteria
-- With version 0.55 we compute everything from white point of view
-- and only at the end we negate the score if black side is asked
----------------------------------------------------------------------------
------ King Safety ------
data KingSafe = KingSafe

instance EvalItem KingSafe where
    evalItem _ p _ = kingSafe p
    evalItemNDL _  = [("kingSafe", (1, (0, 20)))]

-- Rewrite of king safety taking into account number and quality
-- of pieces attacking king neighbour squares
-- This function is almost optimised, it could perhaps be faster
-- if we eliminate the lists
kingSafe :: MyPos -> [Int]
kingSafe !p = [ksafe]
    where !ksafe = mattacs - yattacs
          !freem = popCount1 $ myKAttacs p .&. yoAttacs p `less` me p
          !freey = popCount1 $ yoKAttacs p .&. myAttacs p `less` yo p
          flag k a = if k .&. a /= 0 then 1 else 0
          qual k a = popCount1 $ k .&. a
          flagYo = flag (myKAttacs p)
          flagMe = flag (yoKAttacs p)
          qualYo = qual (myKAttacs p)
          qualMe = qual (yoKAttacs p)
          ($:) = flip ($)
          attsm = map (p $:) [ myPAttacs, myNAttacs, myBAttacs, myRAttacs, myQAttacs, myKAttacs ]
          !ixm = max 0 $ min 63 $ fm * cm - freey
          attsy = map (p $:) [ yoPAttacs, yoNAttacs, yoBAttacs, yoRAttacs, yoQAttacs, yoKAttacs ]
          !ixy = max 0 $ min 63 $ fy * cy - freem
          !mattacs = attCoef `unsafeAt` ixm
          !yattacs = attCoef `unsafeAt` ixy
          qualWeights = [1, 1, 1, 2, 3, 1]
          !(Flc fm cm) = sumCount flagMe qualMe $ zip attsm qualWeights
          !(Flc fy cy) = sumCount flagYo qualYo $ zip attsy qualWeights

-- To make the sum and count in one pass
data Flc = Flc !Int !Int

{-# INLINE sumCount #-}
sumCount :: (BBoard -> Int) -> (BBoard -> Int) -> [(BBoard, Int)] -> Flc
sumCount flag qual = foldl' (\(Flc f c) (b, i) -> Flc (f + flag b) (c + i * qual b)) (Flc 0 0)

attCoef :: UArray Int Int
attCoef = listArray (0, 63) [ f x | x <- [0..63] ]
    where f :: Int -> Int
          f x = let y = fromIntegral x :: Double in round $ (2.92968750 - 0.03051758*y)*y*y

kingSquare :: BBoard -> BBoard -> Square
kingSquare kingsb colorp = head $ bbToSquares $ kingsb .&. colorp
{-# INLINE kingSquare #-}

------ Material ------
data Material = Material

instance EvalItem Material where
    evalItem _ p _ = materDiff p
    evalItemNDL _  = [("materialDiff", (8, (8, 8)))]

-- We will not count the material incremental, as with the change of values based on stage and
-- position openness this is not feasible anymore
materDiff :: MyPos -> IWeights
materDiff p = [md]
    where !md = mw - mb
          !wp = popCount1 $ pawns p .&. me p
          !bp = popCount1 $ pawns p .&. yo p
          grd = wp + bp
          !mw = materColor p (me p) wp grd
          !mb = materColor p (yo p) bp grd

materColor :: MyPos -> BBoard -> Int -> Int -> Int
materColor p cb !wp !grd = mw
    where !wn = popCount1 $ knights p .&. cb
          !wb = popCount1 $ bishops p .&. cb
          !wr = popCount1 $ rooks p .&. cb
          !wq = popCount1 $ queens p .&. cb
          !wpv = gradedMatVal Pawn (stage p)
          !wnv = gradedMatVal Knight grd
          !wbv = gradedMatVal Bishop grd
          !wrv = gradedMatVal Rook   grd
          !wqv = gradedMatVal Queen  grd
          !mw0 = wp * wpv
          !mw1 = mw0 + wn * wnv
          !mw2 = mw1 + wb * wbv
          !mw3 = mw2 + wr * wrv
          !mw  = mw3 + wq * wqv

------ King openness ------
data KingOpen = KingOpen

instance EvalItem KingOpen where
    evalItem _ p _ = kingOpen p
    evalItemNDL _  = [ ("kingOpenOwn", (-20, (-48, 1))), ("kingOpenAdv", (20, (0, 32)))] 

-- Openness can be tought only with pawns (like we take) or all pieces
-- This function is optimized
kingOpen :: MyPos -> IWeights
kingOpen p = [own, adv]
    where mopbishops = popCount1 $ bishops p .&. yo p
          moprooks   = popCount1 $ rooks p .&. yo p
          mopqueens  = popCount1 $ queens p .&. yo p
          mwb = popCount $ bAttacs paw msq `less` paw
          mwr = popCount $ rAttacs paw msq `less` paw
          yopbishops = popCount1 $ bishops p .&. me p
          yoprooks   = popCount1 $ rooks p .&. me p
          yopqueens  = popCount1 $ queens p .&. me p
          ywb = popCount $ bAttacs paw ysq `less` paw
          ywr = popCount $ rAttacs paw ysq `less` paw
          paw = pawns p
          msq = kingSquare (kings p) $ me p
          ysq = kingSquare (kings p) $ yo p
          comb !oB !oR !oQ !wb !wr = let !v = oB * wb + oR * wr + oQ * (wb + wr) in v
          !own = comb mopbishops moprooks mopqueens mwb mwr
          !adv = comb yopbishops yoprooks yopqueens ywb ywr

------ King on a center file ------
data KingCenter = KingCenter

instance EvalItem KingCenter where
    evalItem _ p _ = kingCenter p
    evalItemNDL _  = [ ("kingCenter", (-120, (-200, 0))) ]

-- This function is optimised
kingCenter :: MyPos -> IWeights
kingCenter p = [ kcd ]
    where kcenter = fileC .|. fileD .|. fileE .|. fileF
          !wkc = popCount1 (kings p .&. me p .&. kcenter) * (brooks + 2 * bqueens - 1)
          !bkc = popCount1 (kings p .&. yo p .&. kcenter) * (wrooks + 2 * wqueens - 1)
          !kcd = interp (stage p) (wkc - bkc)
          !wrooks  = popCount1 $ rooks  p .&. me p
          !wqueens = popCount1 $ queens p .&. me p
          !brooks  = popCount1 $ rooks  p .&. yo p
          !bqueens = popCount1 $ queens p .&. yo p
          interp !x !v = v * stageInterp (1500, 0) (6000, 100) x `div` 100

-- King placement
data KingPlace = KingPlace

instance EvalItem KingPlace where
    evalItem _ p _ = kingPlace p
    evalItemNDL _  = [("kingPlace", (4, (0, 5)))]	-- value is 100x, see kingPlace

kingPlace :: MyPos -> IWeights
kingPlace p = [ kp ]
    where msq = kingSquare (kings p)   white
          ysq = kingSquare (kings p) $ black p
          wkp = whiteKingPos `unsafeAt` msq
          bkp = blackKingPos `unsafeAt` ysq
          kp = interp (stage p) $ (wkp - bkp) * 100	-- because small values
          interp !x !v = v * stageInterp (1000, 0) (3000, 100) x `div` 100
          white = occup p `less` black p

whiteKingPos, blackKingPos :: UArray Square Int
whiteKingPos = array (0, 63) [(sq, kingPlaceBonus sq) | sq <- [0 .. 63]]
blackKingPos = array (0, 63) [(sq, kingPlaceBonus $ mirror sq) | sq <- [0 .. 63]]
    where mirror sq = col + (7 - row) * 8 where (row, col) = sq `divMod` 8

-- Seen from white side; if negative, it's a penalty, of course
-- The value will be multiplied by a centipawn parameter, like 50 cp
kingPlaceBonus 0 = 2
kingPlaceBonus 1 = 2
kingPlaceBonus 2 = -1
kingPlaceBonus 3 = -2
kingPlaceBonus 4 = -2
kingPlaceBonus 5 = -1
kingPlaceBonus 6 = 2
kingPlaceBonus 7 = 2
kingPlaceBonus sq
    | sq >= 32 = -10
    | sq >= 24 = -6
    | sq >= 16 = -2
    | sq >= 8  = -1 + kingPlaceBonus (sq - 8)

promoW, promoB :: Square -> Square
promoW s = 56 + (s .&. 7)
promoB s =       s .&. 7

------ Mobility ------
data Mobility = Mobility	-- "safe" moves

instance EvalItem Mobility where
    evalItem _ p _ = mobDiff p
    evalItemNDL _  = [ ("mobilityPawn", (40, (10, 150))),
                       ("mobilityKnight", (120, (60, 150))),
                       ("mobilityBishop", (80, (60, 150))),
                       ("mobilityRook", (80, (40, 150))),
                       ("mobilityQueen", (100, (0, 150))) ]

mobilityUnit :: Int
mobilityUnit = 10	-- for 1024 mu per square

mobDiff :: MyPos -> IWeights
mobDiff p = a `seq` n `seq` b `seq` r `seq` q `seq` [a, n, b, r, q]
    where myPa = popCount1 $ myPAttacs p .&. yo p
          wp = me p .&. pawns p
          myPm = pAll1MovesCount White wp (occup p) + pAll2MovesCount White wp (occup p)
          yoPa = popCount1 $ yoPAttacs p .&. me p
          bp = yo p .&. pawns p
          yoPm = pAll1MovesCount Black bp (occup p) + pAll2MovesCount Black bp (occup p)
          a = (3 * (myPa - yoPa) `unsafeShiftR` 1) + myPm - yoPm	-- captures more
          n = (myN - yoN) `unsafeShiftR` mobilityUnit
          b = (myB - yoB) `unsafeShiftR` mobilityUnit
          r = (myR - yoR) `unsafeShiftR` mobilityUnit
          q = (myQ - yoQ) `unsafeShiftR` mobilityUnit
          myPairs = [(myPAttacs p, matPiece White Pawn),
                     (myNAttacs p, matPiece White Knight),
                     (myBAttacs p, matPiece White Bishop),
                     (myRAttacs p, matPiece White Rook),
                     (myQAttacs p, matPiece White Queen),
                     (myKAttacs p, matPiece White King)]
          yoPairs = [(yoPAttacs p, matPiece White Pawn),
                     (yoNAttacs p, matPiece White Knight),
                     (yoBAttacs p, matPiece White Bishop),
                     (yoRAttacs p, matPiece White Rook),
                     (yoQAttacs p, matPiece White Queen),
                     (yoKAttacs p, matPiece White King)]
          myN = sum $ map (\sq -> mobPiece (matPiece White Knight) yoPairs (myAttacs p)
                                           (nAttacs sq `less` me p))
                          $ bbToSquares $ knights p .&. me p
          myB = sum $ map (\sq -> mobPiece (matPiece White Bishop) yoPairs (myAttacs p)
                                           (bAttacs (occup p) sq `less` me p))
                          $ bbToSquares $ bishops p .&. me p
          myR = sum $ map (\sq -> mobPiece (matPiece White Rook) yoPairs (myAttacs p)
                                           (rAttacs (occup p) sq `less` me p))
                          $ bbToSquares $ rooks p .&. me p
          myQ = sum $ map (\sq -> mobPiece (matPiece White Queen) yoPairs (myAttacs p)
                                           (qAttacs (occup p) sq `less` me p))
                          $ bbToSquares $ queens p .&. me p
          yoN = sum $ map (\sq -> mobPiece (matPiece White Knight) myPairs (yoAttacs p)
                                           (nAttacs sq `less` yo p))
                          $ bbToSquares $ knights p .&. yo p
          yoB = sum $ map (\sq -> mobPiece (matPiece White Bishop) myPairs (yoAttacs p)
                                           (bAttacs (occup p) sq `less` yo p))
                          $ bbToSquares $ bishops p .&. yo p
          yoR = sum $ map (\sq -> mobPiece (matPiece White Rook) myPairs (yoAttacs p)
                                           (rAttacs (occup p) sq `less` yo p))
                          $ bbToSquares $ rooks p .&. yo p
          yoQ = sum $ map (\sq -> mobPiece (matPiece White Queen) myPairs (yoAttacs p)
                                           (qAttacs (occup p) sq `less` yo p))
                          $ bbToSquares $ queens p .&. yo p

-- For mobility we have an own unit, call it MU, to take into account the reduction per square
-- when that square is attacked by an enemy and still work with integers.
-- We give 1000 MU for one possible move of a piece on an square which is not attacked by
-- the enemy. If the square is attacked by an enemy piece which we could recapture, we give less:
-- 1024 * ap / op
-- where ap is the value of the enemy piece, op is the value of our piece
-- If we could not recapture, we give only half of that value

-- Mobility for one piece: we need the piece value, the pairs enemy attacs / enemy piece value,
-- own attacs and the possible moves
-- The ratios must be in increasing order, i.e. pawn attacs to queen attacs
mobPiece :: Int -> [(BBoard, Int)] -> BBoard -> BBoard -> Int
mobPiece opv prs oa db = free + att `div` opv
    where (!att, _, !rest) = foldl f ini prs
          ini = (0, 0, db)
          f (!acc, !cov, !rst) (aa, apv) = (acc', cov', rst')
              where !att = (rst .&. aa) `less` cov
                    !pv  = min apv opv
                    -- !rec = pv * (popCount1 $ att .&. oa)	-- not so easy with recaptures
                    -- !nre = pv * (popCount1 $ att `less` oa)
                    !nre = pv * (popCount1 att)		-- so we assume we cannot recapture
                    -- acc' = acc + rec	+ (nre `unsafeShiftR` 1)
                    acc' = acc + (nre `unsafeShiftR` 1)
                    cov' = cov .|. aa
                    rst' = rst `less` aa
          free = popCount1 rest `unsafeShiftL` mobilityUnit

------ Center control ------
data Center = Center

instance EvalItem Center where
    evalItem _ p _ = centerDiff p
    evalItemNDL _  = [("centerAttacs", (8, (0, 8)))]

centerDiff :: MyPos -> IWeights
centerDiff p = [wb]
    where !wb = interp (stage p) (w - b)
          ring0 = 0x0000001818000000
          ring1 = 0x00003C24243C0000
          pr0 = pawns p .&. ring0
          mr0 = (knights p .|. bishops p) .&. ring0
          jr0 = (rooks p .|. queens p) .&. ring0
          wpr0 = popCount1 $! pr0 .&. me p
          bpr0 = popCount1 $! pr0 .&. yo p
          wmr0 = popCount1 $! mr0 .&. me p
          bmr0 = popCount1 $! mr0 .&. yo p
          wjr0 = popCount1 $! jr0 .&. me p
          bjr0 = popCount1 $! jr0 .&. yo p
          wpcr0 = popCount1 $! myPAttacs p .&. ring0	-- attacs in ring 0
          bpcr0 = popCount1 $! yoPAttacs p .&. ring0
          wfcr0 = popCount1 $! ring0 .&. (myNAttacs p .|. myBAttacs p .|. myRAttacs p .|. myQAttacs p)
          bfcr0 = popCount1 $! ring0 .&. (yoNAttacs p .|. yoBAttacs p .|. yoRAttacs p .|. yoQAttacs p)
          wr1 = popCount1 $! ring1 .&. (me p `less` kings p)	-- all in ring 1
          br1 = popCount1 $! ring1 .&. (yo p `less` kings p)
          w = 40 * wpr0 + 20 * wmr0 + 30 * wjr0 + 10 * wpcr0 + 10 * wfcr0 + 10 * wr1
          b = 40 * bpr0 + 20 * bmr0 + 30 * bjr0 + 10 * bpcr0 + 10 * bfcr0 + 10 * br1
          interp !x !v = v * stageInterp (1500, 0) (6000, 100) x `div` 100

------ En prise ------
data EnPrise = EnPrise

instance EvalItem EnPrise where
    evalItem _ p _ = enPrise p
    evalItemNDL _  = [("enPriseFrac", (1, (0, 1)))]

-- We want to calculate fix 10% of the attacked piece and take care that the parameter above
-- is 1, so we must express the bonus in 800 units per pawn
-- The computation then should be: material attacked in cp divided by 10 and multiplied by 8
-- But to simplify the computation we let the difference as it is, resulting in a final
-- percentage of 12,5% of the attacked value
-- But that 12,5% seems too much, we take 1/2 of it, which means ~6%
enPrise :: MyPos -> IWeights
enPrise p = [epp]
    where !ko = popCount1 $ me p .&. knights p .&. yoAttacs p
          !ka = popCount1 $ yo p .&. knights p .&. myAttacs p
          !bo = popCount1 $ me p .&. bishops p .&. yoAttacs p
          !ba = popCount1 $ yo p .&. bishops p .&. myAttacs p
          !ro = popCount1 $ me p .&. rooks p .&. yoAttacs p
          !ra = popCount1 $ yo p .&. rooks p .&. myAttacs p
          !qo = popCount1 $ me p .&. queens p .&. yoAttacs p
          !qa = popCount1 $ yo p .&. queens p .&. myAttacs p
          !po = popCount1 $ me p .&. pawns p .&. yoAttacs p
          !pa = popCount1 $ yo p .&. pawns p .&. myAttacs p
          !k = (ka - ko) * matPiece White Knight
          !b = (ba - bo) * matPiece White Bishop
          !r = (ra - ro) * matPiece White Rook
          !q = (qa - qo) * matPiece White Queen
          !a = (pa - po) * matPiece White Pawn
          !epp = (k + b + r + q + a) `unsafeShiftR` 1

------ Defended and undefended pieces ------
data Defend = Defend

instance EvalItem Defend where
    evalItem _ p _ = defended p
    evalItemNDL _  = [("defendFrac", (1, (0, 1))),
                      ("undefendFrac", (1, (0, 1)))]	-- was 8 2x instead of 1!

-- Same logic for defended as enPrise, but we take 1/4 of the value (half as for attacking)
-- Here the recommended percentage is 5%, we have about 3%
defended :: MyPos -> IWeights
defended p = [ def, undef ]
    where !undef = bundef - wundef	-- negate, it is a penalty!
          !ko = popCount1 $ me p .&. knights p .&. myAttacs p
          !ka = popCount1 $ yo p .&. knights p .&. yoAttacs p
          !bo = popCount1 $ me p .&. bishops p .&. myAttacs p
          !ba = popCount1 $ yo p .&. bishops p .&. yoAttacs p
          !ro = popCount1 $ me p .&. rooks p .&. myAttacs p
          !ra = popCount1 $ yo p .&. rooks p .&. yoAttacs p
          !qo = popCount1 $ me p .&. queens p .&. myAttacs p
          !qa = popCount1 $ yo p .&. queens p .&. yoAttacs p
          !po = popCount1 $ me p .&. pawns p .&. myAttacs p
          !pa = popCount1 $ yo p .&. pawns p .&. yoAttacs p
          !k = (ko - ka) * matPiece White Knight
          !b = (bo - ba) * matPiece White Bishop
          !r = (ro - ra) * matPiece White Rook
          !q = (qo - qa) * matPiece White Queen
          !a = (po - pa) * matPiece White Pawn
          !def = (k + b + r + q + a) `unsafeShiftR` 2
          !kop = kings p .|. pawns p
          !wu = popCount1 $ (me p `less` kop) `less` myAttacs p
          !bu = popCount1 $ (yo p `less` kop) `less` yoAttacs p
          !wus | wu == 0 = 0
               | wu == 1 = 5
               | otherwise = 7 * wu
          !bus | bu == 0 = 0
               | bu == 1 = 5
               | otherwise = 7 * bu
          !wup = popCount1 $ (me p .&. pawns p) `less` myAttacs p
          !bup = popCount1 $ (yo p .&. pawns p) `less` yoAttacs p
          !wups = wup * 5	-- this is 4 mp
          !bups = bup * 5
          !wundef = wus + wups
          !bundef = bus + bups

------ Redundance: bishop pair and rook redundance ------
data Redundance = Redundance

instance EvalItem Redundance where
    evalItem _ p _ = evalRedundance p
    evalItemNDL _  = [("bishopPair",       (320,  (100, 400))),
                      ("redundanceRook",   (-104,  (-150, 0))) ]

-- This function is optimised
evalRedundance :: MyPos -> [Int]
evalRedundance p = [bp, rr]
    where !wbl = bishops p .&. me p .&. lightSquares
          !wbd = bishops p .&. me p .&. darkSquares
          !bbl = bishops p .&. yo p .&. lightSquares
          !bbd = bishops p .&. yo p .&. darkSquares
          !bpw = popCount1 wbl .&. popCount1 wbd	-- tricky here: exact 1 and 1 is ok
          !bpb = popCount1 bbl .&. popCount1 bbd	-- and here
          !bp  = bpw - bpb
          !wro = rooks p .&. me p
          !bro = rooks p .&. yo p
          !wrr = popCount1 wro `unsafeShiftR` 1	-- tricky here: 2, 3 are the same...
          !brr = popCount1 bro `unsafeShiftR` 1	-- and here
          !rr  = wrr - brr

------ Knight & Rook correction according to own pawns ------
data NRCorrection = NRCorrection

instance EvalItem NRCorrection where
    evalItem _ p _ = evalNRCorrection p
    evalItemNDL _  = [("nrCorrection", (0, (0, 8)))]

-- This function seems to be already optimised
evalNRCorrection :: MyPos -> [Int]
evalNRCorrection p = [md]
    where !wpc = popCount1 (pawns p .&. me p) - 5
          !bpc = popCount1 (pawns p .&. yo p) - 5
          !wnp = popCount1 (knights p .&. me p) * wpc * 6	-- 1/16 for each pawn over 5
          !bnp = popCount1 (knights p .&. yo p) * bpc * 6	-- 1/16 for each pawn over 5
          !wrp = - popCount1 (rooks p .&. me p) * wpc * 12	-- 1/8 for each pawn under 5
          !brp = - popCount1 (rooks p .&. yo p) * bpc * 12	-- 1/8 for each pawn under 5
          !md = wnp + wrp - bnp - brp

------ Rook pawn weakness ------
data RookPawn = RookPawn

instance EvalItem RookPawn where
    evalItem _ p _ = evalRookPawn p
    evalItemNDL _  = [("rookPawn", (-64, (-120, 0))) ]

-- This function is already optimised
evalRookPawn :: MyPos -> [Int]
evalRookPawn p = [rps]
    where !wrp = popCount1 $ pawns p .&. me p .&. rookFiles
          !brp = popCount1 $ pawns p .&. yo p .&. rookFiles
          !rps = wrp - brp

------ Pass pawns ------
data PassPawns = PassPawns

instance EvalItem PassPawns where
    evalItem _ p _ = passPawns p
    evalItemNDL _  = [("passPawnBonus", (104,  (   0,  160))),
                      ("passPawn4",     (424,  ( 400,  480))),
                      ("passPawn5",     (520,  ( 520,  640))),
                      ("passPawn6",     (1132, (1100, 1200))),
                      ("passPawn7",     (1920, (1600, 2300))) ]
 
passPawns :: MyPos -> IWeights
passPawns p = [dfp, dfp4, dfp5, dfp6, dfp7]
    where !mfpbb = passed p .&. me p
          !yfpbb = passed p .&. yo p
          !mfp  = popCount1   mfpbb
          !mfp4 = popCount1 $ mfpbb .&. row4m
          !mfp5 = popCount1 $ mfpbb .&. row5m
          !mfp6 = popCount1 $ mfpbb .&. row6m
          !mfp7 = popCount1 $ mfpbb .&. row7m
          !yfp  = popCount1   yfpbb
          !yfp4 = popCount1 $ yfpbb .&. row4y
          !yfp5 = popCount1 $ yfpbb .&. row5y
          !yfp6 = popCount1 $ yfpbb .&. row6y
          !yfp7 = popCount1 $ yfpbb .&. row7y
          !dfp  = mfp  - yfp
          !dfp4 = mfp4 - yfp4
          !dfp5 = mfp5 - yfp5
          !dfp6 = mfp6 - yfp6
          !dfp7 = mfp7 - yfp7
          (!row4m, !row5m, !row6m, !row7m, !row4y, !row5y, !row6y, !row7y)
              | moving p == White = (row4, row5, row6, row7, row5, row4, row3, row2)
              | otherwise         = (row5, row4, row3, row2, row4, row5, row6, row7)

-- Pawn end games are treated specially
-- We consider escaped passed pawns in 2 situations:
-- pawn race: when both colors have at least one escaped pp
-- winning promotion: when only one side has it
-- Both are only valid in pawn endings
-- The pawn race is tricky when equal:
-- 1. What if the first promoting part gives check (or even mate)? Or what if after both promotions,
-- the first one can check and evntually capture the opposite queen? We just hope this situations are
-- 2. What about the rest of pawns? Here we make a trick: we shift the passed
-- pawns virtually one row back, which gives less points for the possibly remaining
-- passed pawns - now with queens)
pawnEndGame :: MyPos -> Maybe (Int, [Int])
pawnEndGame p
    | null mescds && null yescds             = Nothing
    | not (null mescds) && not (null yescds) = Just (dpr, [dpr])
    | not (null mescds)                      = Just (myrace, [myrace])
    | not (null yescds)                      = Just (yorace, [yorace])
    -- | -- here we will consider what is with 2 passed pawns which are far enough from each other
    where !mfpbb = passed p .&. me p
          !yfpbb = passed p .&. yo p
          !myking = kingSquare (kings p) (me p)
          !yoking = kingSquare (kings p) (yo p)
          (escMe, escYo, maDiff)
              | moving p == White = (escMeWhite yoking, escYoBlack myking,   mater p)
              | otherwise         = (escMeBlack yoking, escYoWhite myking, - mater p)
          mpsqs  = map escMe $ bbToSquares mfpbb	-- my pp squares & distances to promotion
          !mescds = map snd $ filter fst mpsqs		-- my escaped passed pawns
          ypsqs  = map escYo $ bbToSquares yfpbb	-- your pp squares & distances to promotion
          !yescds = map snd $ filter fst ypsqs		-- your escaped passed pawns
          mesc = not . null $ mescds
          yesc = not . null $ yescds
          dpr | mim < miy     =  promoBonus - distMalus mim
              | mim > miy + 1 = -promoBonus + distMalus miy
              | otherwise     =  withQueens     -- Here: this is more complex, e.g. if check while promoting
                                       -- or direct after promotion + queen capture?
          (mim, msq) = minimumBy (comparing snd) mescds      -- who is promoting first?
          (miy, ysq) = minimumBy (comparing snd) yescds
          myrace =  promoBonus - distMalus mim
          yorace = -promoBonus + distMalus miy
          {--
          dpp | mesc      =  promoBonus - distMalus mim        -- only because we know promo == True
              | otherwise = -promoBonus + distMalus miy
              where (mim, _) = minimumBy (comparing snd) mescds      -- who is promoting first?
                    (miy, _) = minimumBy (comparing snd) yescds
          --}
          promoBonus = 1000     -- i.e. almost a queen (here the unit is 1 cp)
          distMalus x = unsafeShiftL x 3        -- to bring at least 8 cp per move until promotion
          -- We try to estimate static what will be after promotions of both queens
          -- This will be another specialized evaluation function...
          -- But now we consider only the material difference (which consists only of pawns)
          withQueens = maDiff
          -- This one is for prunning: so match we can win at most
          -- yoPawnCount = popCount1 $ pawns p .&. yo p
          -- speg = simplePawnEndGame p	-- just to see how it works...
 
escMeWhite :: Square -> Square -> (Bool, (Square, Int))
escMeWhite !ksq !psq = (esc, (psq, dis))
    where !tsq = promoW psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq
 
escYoWhite :: Square -> Square -> (Bool, (Square, Int))
escYoWhite !ksq !psq = (esc, (psq, dis))
    where !tsq = promoW psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq - 1       -- because we move
 
escMeBlack :: Square -> Square -> (Bool, (Square, Int))
escMeBlack !ksq !psq = (esc, (psq, dis))
    where !tsq = promoB psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq
 
escYoBlack :: Square -> Square -> (Bool, (Square, Int))
escYoBlack !ksq !psq = (esc, (psq, dis))
    where !tsq = promoB psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq - 1       -- because we move

simplePawnEndGame :: MyPos -> Int
simplePawnEndGame p = d
    where !d = mepv - yopv
          !mepv = simplePvMe $ me p .&. pawns p
          !yopv = simplePvYo $ yo p .&. pawns p
          (simplePvMe, simplePvYo) | moving p == White = (simplePvWhite, simplePvBlack)
                                   | otherwise         = (simplePvBlack, simplePvWhite)

-- Just a simple weighted count
simplePvWhite :: BBoard -> Int
simplePvWhite !bb = pv
    where !pv = 100 * pc
          !pc0 = popCount1 $ bb  .&. band
          !bb1 = bb  `unsafeShiftL` 16
          !pc1 = popCount1 $ bb1 .&. band
          !bb2 = bb1 `unsafeShiftL` 16
          !pc2 = popCount1 $ bb2 .&. band
          !pc  = (pc0 `unsafeShiftL` 2) + (pc1 `unsafeShiftL` 1) + pc2
          band = 0x00FFFF0000000000	-- row 6 and 7

simplePvBlack :: BBoard -> Int
simplePvBlack !bb = pv
    where !pv = 100 * pc
          !pc0 = popCount1 $ bb  .&. band
          !bb1 = bb  `unsafeShiftR` 16
          !pc1 = popCount1 $ bb1 .&. band
          !bb2 = bb1 `unsafeShiftR` 16
          !pc2 = popCount1 $ bb2 .&. band
          !pc  = (pc0 `unsafeShiftL` 2) + (pc1 `unsafeShiftL` 1) + pc2
          band = 0x0000000000FFFF00	-- row 2 and 3

halfPawnMax :: Int -> Int -> Int
halfPawnMax mx d
    | steps > mx = 100 * mx
    | otherwise  = 100 * steps
    where steps = (d + 1) `unsafeShiftR` 1
--------------------------------------
