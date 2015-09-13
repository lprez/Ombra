module Graphics.Rendering.Ombra.Draw (
        runDrawCtx,
        execDrawCtx,
        evalDrawCtx,
        drawInit,
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
) where

import Graphics.Rendering.Ombra.Draw.Internal
import Graphics.Rendering.Ombra.Internal.GL

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
