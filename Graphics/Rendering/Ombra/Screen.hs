module Graphics.Rendering.Ombra.Screen (
        MonadScreen(..),
        setViewport
) where

import Graphics.Rendering.Ombra.Internal.GL

class (GLES, Monad m) => MonadScreen m where
        currentViewport :: m (Int, Int)
        -- | Resize the drawing space.
        resizeViewport :: Int -> Int -> m ()

setViewport :: (GLES, MonadGL m) => Int -> Int -> m ()
setViewport w h = gl $ viewport 0 0 (fromIntegral w) (fromIntegral h)
