{-# LANGUAGE NullaryTypeClasses, MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Backend.GLFW (
       makeContext
) where
        
import Control.Concurrent
import Graphics.Rendering.Ombra.Backend
import Graphics.Rendering.Ombra.Backend.OpenGL.GLES
import Graphics.UI.GLFW

instance SafeFork where
        safeFork = Just sfork

sfork :: MVar () -> (IO () -> IO ThreadId) -> IO () -> IO ThreadId
sfork sem fork thread = do mwin <- getCurrentContext
                           fork . drawTo sem $
                                   case mwin of
                                       Just win ->
                                               do makeContextCurrent $ Just win
                                                  thread
                                                  makeContextCurrent Nothing
                                       Nothing -> thread
