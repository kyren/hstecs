{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Tecs.Definitions

class Monad m => HackMemory m where
  peek :: Word16 -> m Word16
  poke :: Word16 -> Word16 -> m ()

type STHackArray s = STUArray s Word16 Word16

newtype HackST s a = HackST (ReaderT (STHackArray s) (ST s) a)
  deriving (Monad, Applicative, Functor)

instance HackMemory (HackST s) where
  peek i = HackST $ do
    arr <- ask
    lift $ readArray arr i

  poke i v = HackST $ do
    arr <- ask
    lift $ writeArray arr i v
    return ()

runHackST :: (forall s. HackST s a) -> a
runHackST hst = runST $ go hst
  where
    go (HackST hacks) = do
      arr <- newArray (0, maximumVariableMemory) 0 :: ST s (STHackArray s)
      runReaderT hacks arr
