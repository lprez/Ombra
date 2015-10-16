module Graphics.Rendering.Ombra.Draw (
        refDrawCtx,
        runDrawCtx,
        execDrawCtx,
        evalDrawCtx,
        drawInit,
        drawState,
        drawBegin,
        drawLayer,
        drawEnd,
        drawGet,
        removeGeometry,
        removeTexture,
        removeProgram,
        textureUniform,
        textureSize,
        resizeViewport,
        renderLayer,
        gl
) where

import Data.IORef
import Graphics.Rendering.Ombra.Draw.Internal
import Graphics.Rendering.Ombra.Internal.GL

-- | Run a Draw action using an IORef and a context.
refDrawCtx :: GLES => Ctx -> Draw a -> IORef DrawState -> IO a
refDrawCtx ctx d ref = do state <- readIORef ref
                          (ret, state') <- runDrawCtx ctx d state
                          writeIORef ref state'
                          return ret

runDrawCtx :: GLES
           => Ctx               -- ^ Context (use the appropriate backend
                                -- functions)
           -> Draw a            -- ^ Draw action
           -> DrawState         -- ^ State (create it with 'drawState')
           -> IO (a, DrawState)
runDrawCtx ctx d = flip evalGL ctx . runDraw d

execDrawCtx :: GLES => Ctx -> Draw a -> DrawState -> IO DrawState
execDrawCtx ctx d = flip evalGL ctx . execDraw d

evalDrawCtx :: GLES => Ctx -> Draw a -> DrawState -> IO a
evalDrawCtx ctx d = flip evalGL ctx . evalDraw d
