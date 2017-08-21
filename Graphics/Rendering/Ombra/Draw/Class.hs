{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Draw.Class (
        MonadDraw(..),
        -- MonadDrawToTexture(..),
) where

import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.Texture.Draw
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Screen

class (MonadGeometry m, MonadProgram m, MonadTexture m, MonadScreen m) =>
        MonadDraw o m

{-
class MonadTexture m => MonadDrawToTexture o m where
        non deve essere possibile creare i buffer dentro Draw normale, dato
        che t non è con forall
        drawToTexture :: FragmentShaderOutput o
                      => Int
                      -> Int
                      -> GBuffer t o
                      -> DepthBuffer??
                      -> ((MonadDraw o m, MonadTexture  => m a)
                      -> (forall t. TexBufs t o -> m b)
                      -> m b
-}
