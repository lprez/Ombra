{-# LANGUAGE MultiParamTypeClasses, RankNTypes, DefaultSignatures #-}

module Graphics.Rendering.Ombra.Draw.Class (
        MonadDraw(..),
        MonadDrawBuffers(..),
        MonadRead(..)
) where

import Data.Word

import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Internal.GL (MonadGL)
import Graphics.Rendering.Ombra.Texture.Draw
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Screen
import Graphics.Rendering.Ombra.Vector

-- | Monads that can be used to draw 'Image's.
class ( MonadGeometry (m o)
      , MonadProgram (m o)
      , MonadTexture (m o)
      , MonadScreen (m o)
      ) => MonadDraw o m where
        drawGeometry :: GeometryVertex g => Geometry g -> m o ()
        default drawGeometry :: (MonadGL (m o), GeometryVertex g)
                             => Geometry g
                             -> m o ()
        drawGeometry = defaultDrawGeometry
        withColorMask :: (Bool, Bool, Bool, Bool) -> m o a -> m o a
        withDepthTest :: Bool -> m o a -> m o a
        withDepthMask :: Bool -> m o a -> m o a

-- | Monads that support drawing to 'GBuffer's and 'DepthBuffer's.
class MonadDrawBuffers m where
        -- | Create a 'GBuffer' and a 'DepthBuffer' and draw something to them.
        createBuffers :: Int
                      -> Int
                      -> GBufferInfo o          -- ^ The buffer that will
                                                -- contain the output of the
                                                -- fragment shader.
                      -> DepthBufferInfo        -- ^ The buffer that contains
                                                -- the depth (and stencil)
                                                -- values.
                      -> m o a                  -- ^ Initializer.
                      -> m o' (a, BufferPair o)
        createGBuffer :: GBufferInfo o
                      -> DepthBuffer
                      -> m o a
                      -> m o' (a, BufferPair o)
        createDepthBuffer :: GBuffer o
                          -> DepthBufferInfo
                          -> m o a
                          -> m o' (a, BufferPair o)
        -- | Draw an image to some buffers.
        drawBuffers :: BufferPair o
                    -> m o a                    -- ^ Image to draw to the
                                                -- buffers.
                    -> m o' a

class MonadDraw o m => MonadRead o m where
        -- | Read a rectangle of pixel colors from the screen (or texture).
        readColor :: (Int, Int, Int, Int) -> m o [Color]
        -- | 'readColor' variant that read color vectors.
        readColorFloat :: (Int, Int, Int, Int) -> m o [Vec4]
        -- | Read a rectangle of pixel depths from the screen (or texture).
        -- Not supported on WebGL!
        readDepth :: (Int, Int, Int, Int) -> m o [Word16]
        -- | 'readDepth' variants that read floats. Not supported on WebGL as well.
        readDepthFloat :: (Int, Int, Int, Int) -> m o [Float]
        -- | Read a rectangle of stencil values from the screen (or texture).
        -- Not supported on WebGL!
        readStencil :: (Int, Int, Int, Int) -> m o [Word8]
