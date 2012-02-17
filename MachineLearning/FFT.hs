{-# LANGUAGE BangPatterns #-}

-- Taken from C++ implementation http://www.relisoft.com/science/Physics/fft.html
-- Truly worth the read!!! Implementation of standard nlogn butterfly inplace fft.

module FFT (runFFT, initDFT, FFTStore, runFFT') where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as M
import Data.Complex
import Data.Bits
import Control.Monad
import Control.Monad.ST
import System.IO.Unsafe

-- Assumes Length is Power of 2
runFFT :: V.Vector Double -> V.Vector (Complex Double)
runFFT vec = runFFT' store vec where
    pnts = V.length vec
    store = initDFT pnts


logPoints :: Int -> Int
logPoints points = shiftFunc (points - 1) where
    shiftFunc !0 = 0
    shiftFunc !x = shiftFunc $ x `shiftR` 1

exps :: Int -> V.Vector (V.Vector (Complex Double))
exps points = vector where
    logP = logPoints points
    
    vector :: V.Vector (V.Vector (Complex Double))
    vector = runST $ do
        container <- GM.new logP
        
        let 
            go1 !l_2 !i = if (i < 0) then return () else do
                let vec = go2 i
                GM.write container i vec
                go1 (l_2 * 2) (i - 1)

            go2 !l_2 = runST $ do
                sub <- GM.new points
                forM_ [0..(points - 1)] $ \i -> do
                    GM.write sub i (wComp (fromIntegral i) (fromIntegral l_2))
                G.freeze sub

        go1 2 (logPoints points)
        G.freeze container

    wComp :: Double -> Double -> Complex Double
    wComp !i !l_2 = cos (minusTwoPi * i / l_2) :+ (-1 * sin (minusTwoPi * i / l_2))
    minusTwoPi = -6.283185307179586

bitRev :: Int -> V.Vector Int
bitRev points = runST $ do
    container <- GM.new points
    let
        halfPoints = points `div` 2
        go1 !i !rev = if (i < 0) then return () else do
            GM.write container i rev
            let newRev = go2 halfPoints rev
            go1 (i - 1) newRev
            
        go2 !mask !rev | rev >= mask = go2 (rev - mask) (mask `shiftR` 1)
                       | otherwise   = rev + mask

    go1 (points - 2) 0
    GM.write container (points - 1) (points - 1)
    G.freeze container


data FFTStore = FFTStore {
	bRev   :: V.Vector Int,
	wVec   :: V.Vector (V.Vector (Complex Double)),
	points :: Int
}


initDFT :: Int -> FFTStore
initDFT points = FFTStore (bitRev points) (exps points) points


runFFT' :: FFTStore -> V.Vector Double -> V.Vector (Complex Double)
runFFT' store vector = transform where
    bits = bRev store
    w = wVec store
    pnts = points store
    logP = logPoints pnts

    -- Scary Code, prolly should refactor, but it is pretty imperative
    transform = runST $ do
        x <- GM.new pnts
        forM_ [0..(pnts - 1)] $ \val -> GM.write x (bits V.! val) (vector V.! val :+ 0)
        
        let
            go1 !level !step = if (level > logP) then return () else do
                let
                    increm = (step * 2)
                    go2 !j = if (j == step) then return () else do
                        let
                            u = (w V.! level) V.! j
                            go3 !i = if (i == pnts) then return () else do
                                x_i <- GM.read x i
                                x_i1 <- GM.read x (i + 1)
                                let t = u * x_i1
                                GM.write x (i + step) (x_i - t)
                                GM.write x i (x_i + t)
                                go3 (i + increm)
                        go3 j 
                        go2 (j + 1)
                go2 0
                go1 (level + 1) (step * 2)
        
        go1 1 1
        G.freeze x