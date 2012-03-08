{-# LANGUAGE ViewPatterns, TupleSections #-}

import System.Random
import Data.Array.IO
import Data.IORef
import Control.Monad

swap a i j = do ai <- a ! i
                aj <- a ! j
                writeArray a i aj
                writeArray a j ai

(!) = readArray

genBounded m (pred -> n) = do
    vs <- newListArray (0, n) [0 .. n] :: IO (IOUArray Int Int)
    ds <- newArray (0, n) 0 :: IO (IOUArray Int Int)
    k <- newIORef n
    es <- newIORef []
    let a f = do c <- randomRIO (0, f) >>= swap vs f >> (vs ! f)
                 p <- readIORef k >>= randomRIO . (f + 1, ) >>= (vs !)
                 modifyIORef es ((p, c) :)
                 (succ -> d) <- ds ! p
                 writeArray ds c d
                 if d >= m then readIORef k >>= swap vs f >> modifyIORef k pred
                           else return ()
    forM_ [n - 1, n - 2 .. 0] a
    readIORef es >>= return
