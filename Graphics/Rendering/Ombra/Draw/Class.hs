{-# LANGUAGE MultiParamTypeClasses, TypeFamilies,
             FunctionalDependencies, FlexibleContexts #-}


module Graphics.Rendering.Ombra.Draw.Class (
        MonadDraw(..),
        MonadRead(..),
        switchStateAuto
) where

import Data.Word

import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Internal.GL (MonadGL)
import Graphics.Rendering.Ombra.Draw.State
import Graphics.Rendering.Ombra.Texture.Draw
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Screen
import Graphics.Rendering.Ombra.Vector

class MonadScreen m => MonadDraw o m | m -> o where
        type BufferDraw o' m :: * -> *
        currentState :: m (DrawState o)
        switchState :: Bool -> DrawState o -> [DrawChange] -> m ()
        -- | Clear the color buffer.
        clearColor :: m ()
        clearColor = clearColorWith $ Vec4 0 0 0 1
        -- | Clear the color buffer filling it with the given color.
        clearColorWith :: Vec4 -> m ()
        -- | Clear the depth buffer.
        clearDepth :: m ()
        clearDepth = clearDepthWith 1
        -- | Clear the depth buffer filling it with the given value.
        clearDepthWith :: Double -> m ()
        -- | Clear the stencil buffer.
        clearStencil :: m ()
        clearStencil = clearStencilWith 0
        -- | Clear the stencil buffer filling it with the given value.
        clearStencilWith :: Int -> m ()
        -- | Create a 'GBuffer' and a 'DepthBuffer' and perform a draw action
        -- with them.
        createBuffers :: ( FragmentShaderOutput o'
                         , MonadDraw o' (BufferDraw o' m)
                         )
                      => Int                    -- ^ Width.
                      -> Int                    -- ^ Height.
                      -> GBufferInfo o'         -- ^ The buffer that will
                                                -- contain the output of the
                                                -- fragment shader.
                      -> DepthBufferInfo        -- ^ The buffer that contains
                                                -- the depth (and stencil)
                                                -- values.
                      -> BufferDraw o' m a      -- ^ Initializer.
                      -> m (a, BufferPair o')
        -- | Like 'createBuffers' but uses an already existing 'DepthBuffer'.
        createGBuffer :: ( FragmentShaderOutput o'
                         , MonadDraw o' (BufferDraw o' m)
                         )
                      => GBufferInfo o'
                      -> DepthBuffer
                      -> BufferDraw o' m a
                      -> m (a, BufferPair o')
        -- | Like 'createBuffers' but uses an already existing 'GBuffer'.
        createDepthBuffer :: ( FragmentShaderOutput o'
                             , MonadDraw o' (BufferDraw o' m)
                             )
                          => GBuffer o'
                          -> DepthBufferInfo
                          -> BufferDraw o' m a
                          -> m (a, BufferPair o')
        -- | Draw something to a 'BufferPair'. This does NOT clear the color,
        -- depth or stencil buffer automatically.
        drawBuffers :: ( FragmentShaderOutput o'
                       , MonadDraw o' (BufferDraw o' m)
                       )
                    => BufferPair o'
                    -> BufferDraw o' m a
                    -> m a


class MonadDraw o m => MonadRead o m | m -> o where
        -- | Read a rectangle of pixel colors from the screen (or texture).
        readColor :: (Int, Int, Int, Int) -> m [Color]
        -- | 'readColor' variant that read color vectors.
        readColorFloat :: (Int, Int, Int, Int) -> m [Vec4]
        -- | Read a rectangle of pixel depths from the screen (or texture).
        -- Not supported on WebGL!
        readDepth :: (Int, Int, Int, Int) -> m [Word16]
        -- | 'readDepth' variant that read floats. Not supported on WebGL as well.
        readDepthFloat :: (Int, Int, Int, Int) -> m [Float]
        -- | Read a rectangle of stencil values from the screen (or texture).
        -- Not supported on WebGL!
        readStencil :: (Int, Int, Int, Int) -> m [Word8]

switchStateAuto :: MonadDraw o m => Bool -> DrawState o -> m ()
switchStateAuto draw s' = do s <- currentState
                             let chgs = rootChanges s'
                                 chgs' = concatMap (diffState True s s') chgs
                             switchState draw s' chgs'
