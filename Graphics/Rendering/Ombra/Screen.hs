module Graphics.Rendering.Ombra.Screen (
        MonadScreen(..),
        setViewport
) where

import Graphics.Rendering.Ombra.Internal.GL

class GLES => MonadScreen m where
        currentViewport :: m (Int, Int)
        resizeViewport :: Int -> Int -> m ()

setViewport :: (GLES, MonadGL m) => Int -> Int -> m ()
setViewport w h = gl $ viewport 0 0 (fromIntegral w) (fromIntegral h)
