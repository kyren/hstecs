{-# LANGUAGE RankNTypes #-}

module Tecs.Simulator (
  HackMemory(..),
  HackST,
  runHackST
) where

import Data.Word
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Applicative
import Control.Monad
import Tecs.Definitions

class Monad m => HackMemory m where
  peek :: Word16 -> m Word16
  poke :: Word16 -> Word16 -> m ()

type STHackArray s = STUArray s Word16 Word16

data HackST s a = HackST (STHackArray s -> ST s (a, STHackArray s))

instance Functor (HackST s) where
    fmap = liftM
 
instance Applicative (HackST s) where
    pure  = return
    (<*>) = ap

instance Monad (HackST s) where
  return x = HackST $ \s -> return (x, s)
  (HackST h) >>= f = HackST $ \arr -> do
    (a, newArr) <- h arr
    let (HackST g) = f a
    g newArr

instance HackMemory (HackST s) where
  peek i = HackST $ \arr -> do
    a <- readArray arr i
    return (a, arr)

  poke i v = HackST $ \arr -> do
    writeArray arr i v
    return ((), arr)

runHackST :: (forall s. HackST s a) -> a
runHackST hst = runST $ go hst
  where
    go (HackST hacks) = do
      arr <- newArray (0, maximumVariableMemory) 0
      (r, _) <- hacks arr
      return r
