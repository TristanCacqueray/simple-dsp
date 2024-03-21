-- | This module contains the data representation of samples.
module SimpleDSP.Samples where

import Data.Vector.Storable qualified as SV

-- | The internal representation of samples, presently hardcoded for 44100 Hz.
type Samples = SV.Vector Float

-- | Normalize the current position between 0.0 and 1.0
normalizePos :: Samples -> Int -> Float
normalizePos samples pos = fromIntegral pos / fromIntegral (SV.length samples)
