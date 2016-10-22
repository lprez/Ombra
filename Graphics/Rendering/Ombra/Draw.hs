module Graphics.Rendering.Ombra.Draw (
        Buffer(..),
        refDrawCtx,
        runDrawCtx,
        execDrawCtx,
        evalDrawCtx,
        drawInit,
        drawState,
        clearBuffers,
        drawLayer,
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
import Graphics.Rendering.Ombra.Internal.GL hiding (Buffer)
import Graphics.Rendering.Ombra.Types (Buffer(..))

-- | Run a Draw action using an IORef and a context.
refDrawCtx :: GLES => Ctx -> Draw a -> IORef DrawState -> IO a
refDrawCtx ctx d ref = do state <- readIORef ref
                          (ret, state') <- runDrawCtx ctx d state
                          writeIORef ref state'
                          return ret

runDrawCtx :: Ctx               -- ^ Context (use the appropriate backend
                                -- functions)
           -> Draw a            -- ^ Draw action
           -> DrawState         -- ^ State (create it with 'drawState')
           -> IO (a, DrawState)
runDrawCtx ctx d = flip evalGL ctx . runDraw d

execDrawCtx :: Ctx -> Draw a -> DrawState -> IO DrawState
execDrawCtx ctx d = flip evalGL ctx . execDraw d

evalDrawCtx :: Ctx -> Draw a -> DrawState -> IO a
evalDrawCtx ctx d = flip evalGL ctx . evalDraw d
