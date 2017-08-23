-- |
-- Module:      Graphics.Rendering.Ombra.Draw
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: GHC only

module Graphics.Rendering.Ombra.Draw (
        module Graphics.Rendering.Ombra.OutBuffer,
        Draw,
        DrawState,
        Ctx,
        -- * Running the Draw monad
        refDrawCtx,
        runDrawCtx,
        execDrawCtx,
        evalDrawCtx,
        drawState,
        -- * Draw actions
        MonadDraw,
        MonadDrawBuffers,
        MonadScreen(resizeViewport),
        drawInit,
        drawBuffers,
        drawBuffers',
        clearColor,
        clearDepth,
        clearStencil,
        -- ** Resources
        -- $resources
        ResStatus(..),
        preloadGeometry,
        preloadTexture,
        preloadProgram,
        removeGeometry,
        removeTexture,
        removeProgram,
        checkGeometry,
        checkTexture,
        checkProgram,
        -- * Extensions
        hasVertexArrayObjects,
        hasFloatTextures,
        hasDrawBuffers,
        hasStandardDerivatives,
) where

import Data.IORef
import Graphics.Rendering.Ombra.Backend
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Draw.Monad
import Graphics.Rendering.Ombra.Internal.GL (evalGL)
import Graphics.Rendering.Ombra.OutBuffer
import Graphics.Rendering.Ombra.Shader.Language.Types (GVec4)
import Graphics.Rendering.Ombra.Screen

-- | Run a Draw action using an IORef to hold the state.
refDrawCtx :: GLES
           => Ctx
           -> Draw GVec4 a
           -> IORef DrawState
           -> IO a
refDrawCtx ctx d ref = do state <- readIORef ref
                          (ret, state') <- runDrawCtx ctx d state
                          writeIORef ref state'
                          return ret

runDrawCtx :: Ctx                           -- ^ Context (use the appropriate
                                            -- backend functions)
           -> Draw GVec4 a                  -- ^ Draw action
           -> DrawState                     -- ^ State (create it with
                                            -- 'drawState')
           -> IO (a, DrawState)
runDrawCtx ctx d = flip evalGL ctx . runDraw d

execDrawCtx :: Ctx -> Draw GVec4 a -> DrawState -> IO DrawState
execDrawCtx ctx d = flip evalGL ctx . execDraw d

evalDrawCtx :: Ctx -> Draw GVec4 a -> DrawState -> IO a
evalDrawCtx ctx d = flip evalGL ctx . evalDraw d

-- renderSubLayer :: 

-- $resources
-- In Ombra, GPU resources are allocated when they're needed, and they're kept
-- alive by their corresponding CPU resources. Specifically, these resources are
-- Geometries, Textures and Programs. This means that, when a CPU resource is
-- garbage collected, the GPU resource is also removed. The functions below let
-- you manage allocation and deallocation manually. Note that if you try to use
-- a resource that was deallocated with the remove* functions, it will be
-- allocated again.
