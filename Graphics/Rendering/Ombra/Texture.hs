{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Texture (
        mkTexture,
        emptyTexture
) where

import Data.Hashable

import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Types
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource

-- | Creates a 'Texture' from a list of pixels.
mkTexture :: GLES
          => Int      -- ^ Width.
          -> Int      -- ^ Height.
          -> [Color]  -- ^ List of pixels
          -> Texture
mkTexture w h ps = TextureImage . TexturePixels ps (fromIntegral w)
                                                   (fromIntegral h)
                        $ hash (w, h, ps)

instance GLES => Resource TextureImage LoadedTexture GL where
        loadResource i f = loadTextureImage i $ f . Right -- TODO: err check
        unloadResource _ (LoadedTexture _ _ t) = deleteTexture t

loadTextureImage :: GLES => TextureImage
                 -> (LoadedTexture -> GL ()) -> GL ()
loadTextureImage (TexturePixels ps w h _) f = (>>= f) $
        do t <- emptyTexture
           arr <- liftIO $ encodeColors ps
           texImage2D gl_TEXTURE_2D 0
                      (fromIntegral gl_RGBA)
                      w h 0
                      gl_RGBA
                      gl_UNSIGNED_BYTE
                      arr
           return $ LoadedTexture (fromIntegral w)
                                   (fromIntegral h)
                                   t

emptyTexture :: GLES => GL GL.Texture
emptyTexture = do t <- createTexture
                  bindTexture gl_TEXTURE_2D t
                  param gl_TEXTURE_MAG_FILTER gl_LINEAR
                  param gl_TEXTURE_MIN_FILTER gl_LINEAR
                  param gl_TEXTURE_WRAP_S gl_REPEAT
                  param gl_TEXTURE_WRAP_T gl_REPEAT
                  return t
        where param :: GLES => GLEnum -> GLEnum -> GL ()
              param p v = texParameteri gl_TEXTURE_2D p $ fromIntegral v
