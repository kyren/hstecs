{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tecs.Simulator (
  HackMemory(..),
  HackMemoryST,
  runHackMemoryST,
  HackState(..),
  runHackInstruction
) where

import Data.Int
import Data.Bits
import Data.Array.MArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Tecs.Definitions

class Monad m => HackMemory m where
  peek :: Int16 -> m Int16
  poke :: Int16 -> Int16 -> m ()

type HackMemoryArrayST s = STUArray s Int16 Int16

newtype HackMemoryST s a = HackMemoryST (ReaderT (HackMemoryArrayST s) (ST s) a)
  deriving (Monad, Applicative, Functor)

instance HackMemory (HackMemoryST s) where
  peek i = HackMemoryST $ do
    arr <- ask
    lift $ readArray arr i

  poke i v = HackMemoryST $ do
    arr <- ask
    lift $ writeArray arr i v
    return ()

runHackMemoryST :: (forall s. HackMemoryST s a) -> a
runHackMemoryST hst = runST $ go hst
  where
    go (HackMemoryST hacks) = do
      arr <- newArray (0, maximumVariableMemory) 0 :: ST s (HackMemoryArrayST s)
      runReaderT hacks arr

data HackState = HackState {
    aRegister :: Int16,
    dRegister :: Int16,
    programCounter :: Int16
  }

runHackInstruction :: (HackMemory m) => Operation -> HackState -> m HackState
runHackInstruction (AOperation word) (HackState _ d pc) = return $ HackState word d (pc + 1)
runHackInstruction (COperation comp dest jump) hstate = do
  cr <- calcComp comp hstate
  newState <- putDest dest cr hstate
  return $ doJump jump cr newState

calcComp :: (HackMemory m) => Comp -> HackState -> m Int16
calcComp CompZero _ = return 0
calcComp CompOne _ = return 1
calcComp CompMinusOne _ = return (-1)
calcComp CompD hstate = return $ dRegister hstate
calcComp CompA hstate = return $ aRegister hstate
calcComp CompNotD hstate = return $ complement $ dRegister hstate
calcComp CompNotA hstate = return $ complement $ aRegister hstate
calcComp CompMinusD hstate = return $ negate $ dRegister hstate
calcComp CompMinusA hstate = return $ negate $ aRegister hstate
calcComp CompDPlusOne hstate = return $ dRegister hstate + 1
calcComp CompAPlusOne hstate = return $ aRegister hstate + 1
calcComp CompDMinusOne hstate = return $ dRegister hstate - 1
calcComp CompAMinusOne hstate = return $ aRegister hstate - 1
calcComp CompDPlusA hstate = return $ dRegister hstate + aRegister hstate
calcComp CompDMinusA hstate = return $ dRegister hstate - aRegister hstate
calcComp CompAMinusD hstate = return $ aRegister hstate - dRegister hstate
calcComp CompDAndA hstate = return $ dRegister hstate .&. aRegister hstate
calcComp CompDOrA hstate = return $ dRegister hstate .|. aRegister hstate
calcComp CompM hstate = peek (aRegister hstate)
calcComp CompNotM hstate = liftM complement $ peek (aRegister hstate)
calcComp CompMinusM hstate = liftM negate $ peek (aRegister hstate)
calcComp CompMPlusOne hstate = liftM (+1) $ peek (aRegister hstate)
calcComp CompMMinusOne hstate = liftM (subtract 1) $ peek (aRegister hstate)
calcComp CompDPlusM hstate = liftM (+ dRegister hstate) $ peek (aRegister hstate)
calcComp CompDMinusM hstate = liftM (dRegister hstate -) $ peek (aRegister hstate)
calcComp CompMMinusD hstate = liftM (subtract $ dRegister hstate) $ peek (aRegister hstate)
calcComp CompDAndM hstate = liftM (.&. dRegister hstate) $ peek (aRegister hstate)
calcComp CompDOrM hstate = liftM (.|. dRegister hstate) $ peek (aRegister hstate)

putDest :: (HackMemory m) => Dest -> Int16 -> HackState -> m HackState
putDest DestNULL _ hstate = return hstate
putDest DestM cr hstate = poke (aRegister hstate) cr >> return hstate
putDest DestD cr hstate = return $ hstate { dRegister = cr }
putDest DestMD cr hstate = poke (aRegister hstate) cr >> return (hstate { dRegister = cr })
putDest DestA cr hstate = return $ hstate { aRegister = cr }
putDest DestAM cr hstate = poke (aRegister hstate) cr >> return (hstate { aRegister = cr })
putDest DestAD cr hstate = return $ hstate { aRegister = cr, dRegister = cr }
putDest DestAMD cr hstate = poke (aRegister hstate) cr >> return (hstate { aRegister = cr, dRegister = cr })

jumpCond :: Bool -> HackState -> HackState
jumpCond True hstate = hstate { programCounter = aRegister hstate }
jumpCond False hstate = hstate { programCounter = programCounter hstate + 1 }

doJump :: Jump -> Int16 -> HackState -> HackState
doJump JumpNULL _ = jumpCond False
doJump JumpGT cr = jumpCond (cr > 0)
doJump JumpEQ cr = jumpCond (cr == 0)
doJump JumpGE cr = jumpCond (cr >= 0)
doJump JumpLT cr = jumpCond (cr < 0)
doJump JumpNE cr = jumpCond (cr /= 0)
doJump JumpLE cr = jumpCond (cr <= 0)
doJump JumpMP _ = jumpCond True
