-- | This module contains the logic to visualize samples using OpenGL textures and dear-imgui.
-- This code is mostly adapted from dear-imgui image example.
module SimpleDSP.GUI where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as V
import DearImGui qualified
import DearImGui.Raw qualified
import Foreign qualified
import Graphics.GL qualified as GL

import SimpleDSP.Samples (Samples)

data Texture = Texture
    { textureID :: GL.GLuint
    , textureWidth :: GL.GLsizei
    , textureHeight :: GL.GLsizei
    }
    deriving (Show)

textureSize :: Texture -> DearImGui.ImVec2
textureSize texture =
    DearImGui.ImVec2
        (fromIntegral $ texture.textureWidth)
        (fromIntegral $ texture.textureHeight)

-- | Create a texture pointer in GL memory.
create2DTexture :: Int -> Int -> IO Texture
create2DTexture width height =
    Foreign.alloca \ptr -> do
        GL.glGenTextures 1 ptr
        tID <- Foreign.peek ptr
        pure
            Texture
                { textureID = tID
                , textureWidth = fromIntegral width
                , textureHeight = fromIntegral height
                }

bindTexture :: Texture -> Foreign.Ptr GL.GLubyte -> IO ()
bindTexture texture dataPtr = do
    GL.glEnable GL.GL_TEXTURE_2D
    GL.glBindTexture GL.GL_TEXTURE_2D texture.textureID

    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S GL.GL_REPEAT
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T GL.GL_REPEAT

    GL.glTexImage2D
        GL.GL_TEXTURE_2D
        0
        GL.GL_RGB
        texture.textureWidth
        texture.textureHeight
        0
        GL.GL_RGB
        GL.GL_UNSIGNED_BYTE
        (Foreign.castPtr dataPtr)

{-# ANN drawTexture ("HLint: ignore Avoid lambda" :: String) #-}
drawTexture :: Texture -> IO ()
drawTexture texture =
    Foreign.with (textureSize texture) \sizePtr ->
        Foreign.with (DearImGui.ImVec2 0 0) \uv0Ptr ->
            Foreign.with (DearImGui.ImVec2 1 1) \uv1Ptr ->
                Foreign.with (DearImGui.ImVec4 1 1 1 1) \tintColPtr ->
                    Foreign.with (DearImGui.ImVec4 1 1 1 1) \borderColPtr ->
                        DearImGui.Raw.image openGLtextureID sizePtr uv0Ptr uv1Ptr tintColPtr borderColPtr
  where
    openGLtextureID = Foreign.intPtrToPtr $ fromIntegral texture.textureID

fill :: Texture -> (GL.GLubyte, GL.GLubyte, GL.GLubyte) -> VS.Vector GL.GLubyte
fill texture (r, g, b) =
    VS.generate
        (3 * width * height)
        ( \i ->
            case i `mod` 3 of
                0 -> r
                1 -> g
                2 -> b
                _ -> error "assert: 3-byte pitch"
        )
  where
    width = fromIntegral texture.textureWidth
    height = fromIntegral texture.textureHeight

renderWave :: Texture -> Samples -> VS.Vector GL.GLubyte
renderWave texture samples = runST doRenderWave
  where
    width = fromIntegral texture.textureWidth
    height = fromIntegral texture.textureHeight
    clamp v
        | v > 1.0 = 1.0
        | v < 0.0 = 0.0
        | otherwise = v
    chunkSize = fromIntegral (VS.length samples) / fromIntegral texture.textureWidth :: Float

    doRenderWave :: ST _ (VS.Vector GL.GLubyte)
    doRenderWave = do
        -- initialize pixels
        mv <- V.replicate (3 * width * height) 0

        forM_ [0 .. width - 1] \x -> do
            -- compute the maximum sample value matching the current column
            let start = chunkSize * fromIntegral x
                chunk = VS.slice (floor start) (floor chunkSize) samples
                sampleValue = clamp (VS.foldl' max 0 chunk)
            -- map the value to the texture height
            let sampleHeight = sampleValue * fromIntegral (texture.textureHeight - 1)
            -- draw a bar
            forM_ [0 .. floor sampleHeight] \y -> do
                setPixel mv x (height - 1 - y)

        -- finalize the final texture data
        VS.freeze mv

    setPixel mv x y = do
        let pos = 3 * (y * width + x)
        V.write mv pos 0x7F
        V.write mv (pos + 1) 0x7F
        V.write mv (pos + 2) 0x7F
