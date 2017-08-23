{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}

module Graphics.Rendering.Ombra.Draw.Class (
        MonadDraw(..),
        MonadDrawBuffers(..),
        -- MonadRead(..)
) where

import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.Texture.Draw
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Screen

class (MonadGeometry m, MonadProgram m, MonadTexture m, MonadScreen m) =>
        MonadDraw o m

class MonadDrawBuffers m where
        -- | Draw an image to some buffers instead of the screen.
        drawBuffers :: Int                      -- ^ Width.
                    -> Int                      -- ^ Height.
                    -> GBuffer t s o            -- ^ The buffer that will
                                                -- contain the output of the
                                                -- fragment shader.
                    -> DepthBuffer t' s'        -- ^ The buffer that contains
                                                -- the depth (and stencil)
                                                -- values.
                    -> m o a
                    -> (forall t. UsedGBuffer t o
                               -> UsedDepthBuffer t
                               -> a
                               -> m o' b)
                    -> m o' b

        -- | Use this instead of 'drawBuffers' if you need to reuse the buffers
        -- later. In this case, the buffers are deleted from the GPU when the
        -- 'GBuffer'/'DepthBuffer' is garbage collected.
        drawBuffers' :: Int
                     -> Int
                     -> GBuffer t s o
                     -> DepthBuffer t1 s'
                     -> m o a
                     -> m o' (a, UsedGBuffer t2 o, UsedDepthBuffer t3)

{-
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
-}
