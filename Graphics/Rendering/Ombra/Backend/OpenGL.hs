{-# LANGUAGE NullaryTypeClasses, MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Backend.OpenGL (
       makeContext
) where
        
import Graphics.Rendering.Ombra.Backend
import Graphics.Rendering.Ombra.Backend.OpenGL.GLES

instance SafeFork where
        safeFork = Nothing
