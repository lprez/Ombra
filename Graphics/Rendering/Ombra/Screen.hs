module Graphics.Rendering.Ombra.Screen (
        MonadScreen(..),
        setViewport
) where

import Graphics.Rendering.Ombra.Internal.GL

class (GLES, Monad m) => MonadScreen m where
        currentViewport :: m ((Int, Int), (Int, Int))
        -- | Resize the drawing space.
        resizeViewport :: (Int, Int)    -- ^ (x, y)
                       -> (Int, Int)    -- ^ (width, height)
                       -> m ()

setViewport :: (GLES, MonadGL m) => (Int, Int) -> (Int, Int) -> m ()
setViewport (x, y) (w, h) = gl $ viewport (fromIntegral x) (fromIntegral y)
                                          (fromIntegral w) (fromIntegral h)
