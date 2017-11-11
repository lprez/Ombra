-- |
-- Module:      Graphics.Rendering.Ombra.Draw
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: GHC only

module Graphics.Rendering.Ombra.Draw (
        module Graphics.Rendering.Ombra.OutBuffer,
        Draw,
        Ctx,
        -- * Running the Draw monad
        runDraw,
        -- * Draw actions
        MonadDraw( withColorMask
                 , withDepthTest
                 , withDepthMask
                 , clearColor
                 , clearColorWith
                 , clearDepth
                 , clearDepthWith
                 , clearStencil
                 , clearStencilWith
                 ),
        MonadDrawBuffers(..),
        MonadRead(..),
        MonadScreen(resizeViewport),
        -- ** Culling
        CullFace(..),
        MonadCulling(withCulling),
        -- ** Resources
        -- $resources
        ResStatus(..),
        preloadGeometry,
        preloadTexture,
        removeGeometry,
        removeTexture,
        checkGeometry,
        checkTexture,
) where

import Graphics.Rendering.Ombra.Backend
import Graphics.Rendering.Ombra.Culling.Draw
import Graphics.Rendering.Ombra.Culling.Types
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Draw.Monad
import Graphics.Rendering.Ombra.Internal.GL (evalGL)
import Graphics.Rendering.Ombra.OutBuffer
import Graphics.Rendering.Ombra.Shader.Language.Types (GVec4)
import Graphics.Rendering.Ombra.Screen

-- | Run a 'Draw' program.
runDraw :: GLES
        => Int          -- ^ Viewport width
        -> Int          -- ^ Viewport height
        -> Ctx
        -> Draw GVec4 a
        -> IO a
runDraw w h ctx a = flip evalGL ctx . evalDraw (drawInit >> a) $ drawState w h

-- $resources
-- In Ombra, GPU resources are allocated when they're needed, and they're kept
-- alive by their corresponding CPU resources. Specifically, these resources are
-- Geometries, Textures and Shaders. This means that, when a CPU resource is
-- garbage collected, the GPU resource is also removed. The functions below let
-- you manage allocation and deallocation manually. Note that if you try to use
-- a resource that was deallocated with the remove* functions, it will be
-- allocated again.
