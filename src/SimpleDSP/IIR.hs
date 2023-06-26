{- | This module implements IIR filter.

 See: http://shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
-}
module SimpleDSP.IIR (
    -- * Usage
    filterSamples,
    IIRParams,
    initialIIRState,
    IIRState,

    -- * Design
    lowPassFilter,
) where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Vector.Storable qualified as SV
import SimpleDSP.Samples

data IIRParams = IIRParams
    { b0 :: {-# UNPACK #-} Float
    , b1 :: {-# UNPACK #-} Float
    , b2 :: {-# UNPACK #-} Float
    , a0 :: {-# UNPACK #-} Float
    , a1 :: {-# UNPACK #-} Float
    , a2 :: {-# UNPACK #-} Float
    }
    deriving (Show)

-- | A low-pass filter using cutoff frequency and resonance.
lowPassFilter :: Float -> Float -> IIRParams
lowPassFilter freq q =
    IIRParams
        { b0
        , b1 = 1 - cos w0
        , b2 = b0
        , a0 = 1 + α
        , a1 = -2 * cos w0
        , a2 = 1 - α
        }
  where
    b0 = (1 - cos w0) / 2
    w0 = calcW0 freq
    α = calcAQ w0 q

calcW0 :: Float -> Float
calcW0 freq = 2 * pi * freq / 44100

calcAQ :: Float -> Float -> Float
calcAQ w0 q = sin w0 / (2 * q)

data IIRState = IIRState
    { x0 :: {-# UNPACK #-} Float
    , x1 :: {-# UNPACK #-} Float
    , x2 :: {-# UNPACK #-} Float
    , y0 :: {-# UNPACK #-} Float
    , y1 :: {-# UNPACK #-} Float
    , y2 :: {-# UNPACK #-} Float
    }
    deriving (Show)

initialIIRState :: IIRState
initialIIRState = IIRState 0 0 0 0 0 0

applyIIR :: IIRParams -> Float -> IIRState -> IIRState
applyIIR (IIRParams b0 b1 b2 a0 a1 a2) x0 (IIRState x1 x2 _ y1 y2 _) = newState
  where
    newState = IIRState x0 x1 x2 newSample y1 y2
    newSample = (b0 / a0) * x0 + (b1 / a0) * x1 + (b2 / a0) * x2 - (a1 / a0) * y1 - (a2 / a0) * y2

filterSamplesState :: forall m. (Monad m) => IIRParams -> Samples -> StateT IIRState m Samples
filterSamplesState params = SV.mapM doApplyIIR
  where
    doApplyIIR :: Float -> StateT IIRState m Float
    doApplyIIR curSample = StateT \curState -> do
        let newState = applyIIR params curSample curState
        pure (newState.y0, newState)

filterSamples :: IIRParams -> Samples -> IIRState -> (Samples, IIRState)
filterSamples params samples = runIdentity . runStateT (filterSamplesState @Identity params samples)
