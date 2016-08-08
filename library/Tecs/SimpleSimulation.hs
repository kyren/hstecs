{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tecs.SimpleSimulation (
  runHack
) where

import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Tecs.Definitions
import Tecs.Simulation

type HackMemoryMVector s = VUM.MVector s Int16

newtype HackMemoryST s a = HackMemoryST (ReaderT (HackMemoryMVector s) (ST s) a)
  deriving (Monad, Applicative, Functor)

instance HackMemory (HackMemoryST s) where
  peek i = HackMemoryST $ do
    arr <- ask
    lift $ VUM.read arr (fromIntegral i)

  poke i v = HackMemoryST $ do
    arr <- ask
    lift $ VUM.write arr (fromIntegral i) v
    return ()

runHackMemoryST :: (forall s. HackMemoryST s a) -> a
runHackMemoryST hst = runST $ go hst
  where
    go (HackMemoryST hacks) = do
      arr <- VUM.replicate (fromIntegral maximumVariableMemory) 0 :: ST s (HackMemoryMVector s)
      runReaderT hacks arr

runHack :: [Operation] -> Int -> [Int16] -> [Int16]
runHack ops count rs = runHackMemoryST $ do
    let initialState = HackState 0 0 0
    _ <- go count initialState
    mapM peek rs
  where
    opsVector = V.fromList ops
    go c state
      | c > 0 = runHackOperation (opsVector V.! fromIntegral (programCounter state)) state >>= go (c - 1)
      | otherwise = return state

