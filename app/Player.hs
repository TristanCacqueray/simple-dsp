-- GADTs are required to match on the sdl-mixer sample format
{-# LANGUAGE GADTs #-}

-- | This module implements a proof of concept player that use simple-dsp filter.
module Main where

import Control.Monad.Managed (managed, managed_, runManaged)
import Data.Sequence qualified as Seq
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as V
import DearImGui.OpenGL3 qualified
import DearImGui.SDL qualified
import DearImGui.SDL.OpenGL qualified
import Graphics.GL qualified as GL
import System.Environment (getArgs)
import Witch (from)

import DearImGui
import RIO
import SDL hiding (Texture)

import Data.Coerce (coerce)
import Foreign.C.Types (CFloat (..))
import SimpleDSP.IIR qualified as IIR
import SimpleDSP.IO qualified
import SimpleDSP.Samples (Samples, normalizePos)
import SimpleGUI qualified as GUI

data Player = Player
    { samples :: Samples
    , name :: FilePath
    , pos :: IORef Int
    , running :: IORef Bool
    , gain :: IORef Float
    , freq :: IORef Float
    , res :: IORef Float
    , filterEnabled :: IORef Bool
    , filter :: IORef FilterType
    , iirParams :: IORef IIR.IIRParams
    , iirState :: IORef IIR.IIRState
    , lowRMS :: IORef (IIR.RMSInfo, HistoryVar)
    , midRMS :: IORef (IIR.RMSInfo, HistoryVar)
    , highRMS :: IORef (IIR.RMSInfo, HistoryVar)
    }

data FilterType = LowPass | HighPass | BandPass
    deriving (Eq, Enum, Bounded)

filterName :: FilterType -> Text
filterName = \case
    LowPass -> "low-pass"
    HighPass -> "high-pass"
    BandPass -> "band-pass"

newPlayer :: FilePath -> IO Player
newPlayer fp = do
    samples <- SimpleDSP.IO.decodeFile fp
    Player samples fp
        <$> newIORef 0
        <*> newIORef False
        <*> newIORef 1.0
        <*> newIORef freq
        <*> newIORef res
        <*> newIORef True
        <*> newIORef LowPass
        <*> newIORef (IIR.lowPassFilter freq res)
        <*> newIORef IIR.initialIIRState
        <*> newIORef (IIR.mkRMSInfo (IIR.lowPassFilter 150 2.0), newHistoryVar)
        <*> newIORef (IIR.mkRMSInfo (IIR.bandPassFilter 5800 2.0), newHistoryVar)
        <*> newIORef (IIR.mkRMSInfo (IIR.highPassFilter 12000 10.0), newHistoryVar)
  where
    freq = 440
    res = 1

audioCB :: Player -> V.IOVector Float -> IO ()
audioCB player buffer = do
    currentPos <- readIORef player.pos
    currentGain <- readIORef player.gain
    let size = V.length buffer
        currentSamples = SV.slice currentPos size (samples player)
        newSamples = SV.map (* currentGain) currentSamples

    let updateRMS ioRef = do
            (rms, var) <- readIORef ioRef
            let newRMS = IIR.updateInfo rms newSamples
                newVar = pushHistoryVar newRMS.rmsVolume var
            writeIORef ioRef (newRMS, newVar)

    updateRMS player.lowRMS
    updateRMS player.midRMS
    updateRMS player.highRMS

    -- filter
    filterEnabled <- readIORef player.filterEnabled
    finalSamples <-
        if filterEnabled
            then do
                iirParams <- readIORef player.iirParams
                iirState <- readIORef player.iirState
                let (filteredSamples, newIIRState) = IIR.filterSamples iirParams newSamples iirState
                writeIORef player.iirState newIIRState
                pure filteredSamples
            else pure newSamples

    SV.copy buffer finalSamples
    writeIORef (pos player) (currentPos + size)

mainAudio :: Player -> IO AudioDevice
mainAudio player = do
    (device, _) <-
        openAudioDevice
            OpenDeviceSpec
                { SDL.openDeviceFreq = Mandate 44100
                , SDL.openDeviceFormat = Mandate FloatingLEAudio
                , SDL.openDeviceChannels = Mandate Mono
                , SDL.openDeviceSamples = 44100 `div` 30
                , SDL.openDeviceCallback = \format buffer -> case format of
                    FloatingLEAudio -> audioCB player buffer
                    _ -> error "Unsupported audio format"
                , SDL.openDeviceUsage = ForPlayback
                , SDL.openDeviceName = Nothing
                }
    pure device

usage :: IO Player
usage =
    getArgs >>= \case
        [fp] -> newPlayer fp
        _ -> error "usage: simple-dsp-player FILE"

main :: IO ()
main = do
    player <- usage

    initializeAll

    -- Setup audio thread
    audioDevice <- mainAudio player
    let setPlayState isRunning = do
            setAudioDevicePlaybackState audioDevice $ if isRunning then Play else Pause
    setPlayState =<< readIORef player.running

    -- Handle play/pause button (or space key)
    let togglePlayState = do
            modifyIORef player.running not
            setPlayState =<< readIORef player.running

    -- Handle sample pos (left/right arrow key)
    let jogPos dir = do
            let clamp = min (SV.length player.samples) . max 0
            modifyIORef player.pos \pos -> clamp (pos `dir` 44100)

    -- Handle key event and quit condition
    let keyHandler event
            | isQuit = pure True
            | otherwise = do
                when (keyCode == Just ScancodeSpace) togglePlayState
                when (keyCode == Just ScancodeLeft) (jogPos (-))
                when (keyCode == Just ScancodeRight) (jogPos (+))
                pure False
          where
            keyCode = case eventPayload event of
                KeyboardEvent ke | ke.keyboardEventKeyMotion == Pressed -> Just ke.keyboardEventKeysym.keysymScancode
                _ -> Nothing
            isQuit =
                SDL.eventPayload event == SDL.QuitEvent
                    || keyCode == Just ScancodeEscape

    -- Create sound wave texture
    let withTexture cb = do
            waveTexture <- GUI.create2DTexture 780 240
            SV.unsafeWith (GUI.renderWave waveTexture player.samples) $
                GUI.bindTexture waveTexture

            posTexture <- GUI.create2DTexture 1 240
            SV.unsafeWith (GUI.fill posTexture (0xFF, 0x00, 0x7F)) $
                GUI.bindTexture posTexture

            cb (waveTexture, posTexture)

    -- Handle filter re setting
    let resetFilter = do
            freq <- readIORef player.freq
            res <- readIORef player.res
            filterType <- readIORef player.filter
            let mkFilter = case filterType of
                    LowPass -> IIR.lowPassFilter
                    HighPass -> IIR.highPassFilter
                    BandPass -> IIR.bandPassFilter
            let newParams = mkFilter freq res
            print newParams
            writeIORef player.iirParams newParams

    mainGUI keyHandler withTexture \(waveTexture, posTexture) -> withFullscreen do
        text "simple-dsp-player demo"

        -- player buttons
        isRunning <- readIORef player.running
        let playBtn = if isRunning then "pause" else "start"
        whenM (DearImGui.button playBtn) togglePlayState
        DearImGui.sameLine
        currentPos <- readIORef player.pos
        let uvPos = normalizePos player.samples currentPos
        DearImGui.text (from $ show currentPos)

        void $ DearImGui.sliderFloat "gain" player.gain 0 5

        void $ DearImGui.checkbox "enabled" player.filterEnabled
        DearImGui.sameLine
        currentFilter <- readIORef player.filter
        whenM (DearImGui.beginCombo "##sel" (filterName currentFilter)) do
            forM_ [minBound .. maxBound] \otherFilter -> do
                when (otherFilter /= currentFilter) do
                    whenM (DearImGui.selectable (filterName otherFilter)) do
                        writeIORef player.filter otherFilter
                        resetFilter
            DearImGui.endCombo

        whenM (DearImGui.sliderFloat "freq" player.freq 0 20000) do
            resetFilter
        whenM (DearImGui.sliderFloat "res" player.res 0.001 42) do
            resetFilter

        -- progress
        DearImGui.progressBar uvPos Nothing

        DearImGui.ImVec2 drawPosX drawPosY <- DearImGui.getCursorPos
        GUI.drawTexture waveTexture
        DearImGui.setCursorPos =<< newIORef (DearImGui.ImVec2 (drawPosX - 3 + uvPos * 780) drawPosY)
        GUI.drawTexture posTexture

        renderHistoryVar "LOW" =<< (snd <$> readIORef player.lowRMS)
        renderHistoryVar "MID" =<< (snd <$> readIORef player.midRMS)
        renderHistoryVar "HIGH" =<< (snd <$> readIORef player.highRMS)

        -- current wave
        let sampleList :: [Float]
            sampleList = SV.toList $ SV.slice currentPos (44100 `div` 10) player.samples
        DearImGui.plotLines "samples" $ coerce sampleList

-- sdl bootstrap adapted from the dear-imgui readme.
mainGUI :: (Event -> IO Bool) -> _ -> (_ -> IO ()) -> IO ()
mainGUI eventHandler withTextures renderUI = do
    runManaged do
        window <- do
            let title = "simple-dsp-demo"
            let config = defaultWindow{windowGraphicsContext = OpenGLContext defaultOpenGL}
            managed $ bracket (createWindow title config) destroyWindow
        glContext <- managed $ bracket (glCreateContext window) glDeleteContext
        _ <- managed $ bracket createContext destroyContext
        _ <- managed_ $ bracket_ (DearImGui.SDL.OpenGL.sdl2InitForOpenGL window glContext) DearImGui.SDL.sdl2Shutdown
        _ <- managed_ $ bracket_ DearImGui.OpenGL3.openGL3Init DearImGui.OpenGL3.openGL3Shutdown

        liftIO $ withTextures \textures -> mainLoop window (renderUI textures) eventHandler

mainLoop :: Window -> IO () -> (Event -> IO Bool) -> IO ()
mainLoop window renderUI eventHandler = unlessQuit do
    DearImGui.OpenGL3.openGL3NewFrame
    DearImGui.SDL.sdl2NewFrame
    DearImGui.newFrame
    renderUI
    GL.glClear GL.GL_COLOR_BUFFER_BIT
    DearImGui.render
    DearImGui.OpenGL3.openGL3RenderDrawData =<< getDrawData
    SDL.glSwapWindow window
    mainLoop window renderUI eventHandler
  where
    unlessQuit action = do
        shouldQuit <- traverse eventHandler =<< DearImGui.SDL.pollEventsWithImGui
        unless (or shouldQuit) action

-- Copied from AF
data HistoryVar = HistoryVar
    { value :: Float
    , history :: Seq CFloat
    }

historySize :: Int
historySize = 128

newHistoryVar :: HistoryVar
newHistoryVar = HistoryVar 0 (Seq.replicate historySize 0)

-- | Set the variable value and update the history.
pushHistoryVar :: Float -> HistoryVar -> HistoryVar
pushHistoryVar newValue hvar = newHVar
  where
    newHVar
        | prevValue == newValue = hvar
        | otherwise = HistoryVar newValue newHistory
    newHistory
        | Seq.length hvar.history < historySize = newSeq
        | otherwise = Seq.drop 1 newSeq
    newSeq = hvar.history Seq.|> CFloat newValue
    prevValue = case hvar.history of
        _ Seq.:|> (CFloat x) -> x
        _ -> 0

renderHistoryVar :: (MonadUnliftIO m) => Text -> HistoryVar -> m ()
renderHistoryVar name hvar = do
    -- current value
    void $! DearImGui.plotLines "##" (toList hvar.history)
    -- history
    DearImGui.sameLine
    void $! DearImGui.text $ name <> ": " <> from (show hvar.value)
