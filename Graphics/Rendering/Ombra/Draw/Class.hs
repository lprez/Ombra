{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}

module Graphics.Rendering.Ombra.Draw.Class (
        MonadDraw(..),
        MonadDrawBuffers(..),
        MonadRead(..)
) where

import Data.Word

import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Geometry.Draw
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
        withColorMask :: (Bool, Bool, Bool, Bool) -> m o a -> m o a
        withDepthTest :: Bool -> m o a -> m o a
        withDepthMask :: Bool -> m o a -> m o a

-- | Monads that support drawing to 'GBuffer's and 'DepthBuffer's.
class MonadDrawBuffers m where
        -- | Draw an image to some buffers.
        drawBuffers :: Int                      -- ^ Width of the buffers.
                    -> Int                      -- ^ Height of the buffers.
                    -> Either (GBuffer t o)
                              (GBufferInfo o)   -- ^ The buffer that will
                                                -- contain the output of the
                                                -- fragment shader. Either a
                                                -- previously used buffer, or
                                                -- the info of a new buffer
                                                -- that drawBuffers will create.
                                                -- Do not reuse buffers with a
                                                -- different width or height
                                                -- than the one you specified
                                                -- with the previous argument.
                    -> Either (DepthBuffer t')
                              DepthBufferInfo   -- ^ The buffer that contains
                                                -- the depth (and stencil)
                                                -- values.
                    -> m o a                    -- ^ Image to draw to the
                                                -- buffers.
                    -> (forall t. GBuffer t o -> DepthBuffer t -> a -> m o' b)
                    -> m o' b

        -- | Use this instead of 'drawBuffers' if you need to reuse the newly
        -- created buffers layer. They will be deleted from the GPU when the
        -- 'GBuffer'/'DepthBuffer' is garbage collected.
        drawBuffers' :: Int
                     -> Int
                     -> Either (GBuffer t o) (GBufferInfo o)
                     -> Either (DepthBuffer t1) DepthBufferInfo
                     -> m o a
                     -> m o' (a, GBuffer t2 o, DepthBuffer t3)

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
