-- | This module contains the logic to load a sound file.
module SimpleDSP.IO where

import Data.ByteString (toStrict)
import Data.ByteString.Internal (toForeignPtr0)
import Data.Vector.Storable qualified as SV
import Foreign (Storable (sizeOf))
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import System.Exit (ExitCode (ExitSuccess))

import System.Process.Typed (proc, readProcessStdout)

import SimpleDSP.Samples

-- | Normalize input file into a 44100 mono float array.
decodeFile :: FilePath -> IO Samples
decodeFile fname = do
    -- putStrLn $ "Running: ffmpeg " <> unwords args
    (ExitSuccess, pcmBuf) <- readProcessStdout $ proc "ffmpeg" args
    let (wordPtr, wordSZ) = toForeignPtr0 (toStrict pcmBuf)
        pcmPtr = castForeignPtr wordPtr :: ForeignPtr Float
        pcmSZ = wordSZ `div` sizeOf (0 :: Float)
        arr = SV.unsafeFromForeignPtr0 pcmPtr pcmSZ
    pure arr
  where
    args =
        ["-hide_banner", "-loglevel", "info"] -- quiet
            <> ["-i", fname] -- input
            -- <> ["-to", "30"] -- limit to 30sec
            <> ["-ac", "1"] -- convert to mono
            <> ["-ar", "44100"] -- sample at 44100 (fit for 60 fps)
            <> ["-f", "f32le"] -- use float
            <> ["-"] -- output
