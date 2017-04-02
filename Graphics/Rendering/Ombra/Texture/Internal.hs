{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Texture.Internal where

import Control.Monad (when)
import Data.Hashable
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource

-- | A texture.
data Texture = TextureImage TextureImage
             | TextureLoaded LoadedTexture
             deriving Eq
             
data TextureImage = TexturePixels Bool [[Color]] (Filter, Maybe Filter) Filter
                                  GLSize GLSize Int
                  | TextureRaw Bool [UInt8Array] (Filter, Maybe Filter) Filter
                               GLSize GLSize Int
                  | TextureFloat [Float] (Filter, Maybe Filter) Filter
                                 GLSize GLSize Int

data Filter = Linear    -- ^ Average of the four nearest pixels.
            | Nearest   -- ^ Nearest pixel.
            deriving Eq

data LoadedTexture = LoadedTexture GLSize GLSize GL.Texture

instance Hashable TextureImage where
        hashWithSalt salt tex = hashWithSalt salt $ textureHash tex

instance Eq TextureImage where
        (TexturePixels _ _ _ _ _ _ h) == (TexturePixels _ _ _ _ _ _ h') = h == h'
        (TextureRaw _ _ _ _ _ _ h) == (TextureRaw _ _ _ _ _ _ h') = h == h'
        (TextureFloat _ _ _ _ _ h) == (TextureFloat _ _ _ _ _ h') = h == h'
        _ == _ = False

instance GLES => Eq LoadedTexture where
        LoadedTexture _ _ t == LoadedTexture _ _ t' = t == t'

textureHash :: TextureImage -> Int
textureHash (TexturePixels _ _ _ _ _ _ h) = h
textureHash (TextureRaw _ _ _ _ _ _ h) = h
textureHash (TextureFloat _ _ _ _ _ h) = h

instance GLES => Resource TextureImage LoadedTexture GL where
        loadResource i = Right <$> loadTextureImage i
        unloadResource _ (LoadedTexture _ _ t) = deleteTexture t

loadTextureImage :: GLES => TextureImage -> GL LoadedTexture
loadTextureImage (TexturePixels g pss min mag w h hash) =
        do arr <- mapM (\ps -> liftIO . encodeUInt8s .
                               take (fromIntegral $ w * h * 4) $
                               ps >>= \(Color r g b a) -> [r, g, b, a]) pss
           loadTextureImage $ TextureRaw g arr min mag w h hash
loadTextureImage (TextureRaw g arrs min mag w h _) =
        do t <- emptyTexture min mag
           mapM_ (\(arr, l) -> texImage2DUInt gl_TEXTURE_2D l
                                              (fromIntegral gl_RGBA)
                                              w h 0
                                              gl_RGBA
                                              gl_UNSIGNED_BYTE
                                              arr
                 )
                 (zip arrs [0 ..])
           when g $ generateMipmap gl_TEXTURE_2D
           return $ LoadedTexture (fromIntegral w)
                                  (fromIntegral h)
                                  t
loadTextureImage (TextureFloat ps min mag w h hash) =
        do arr <- liftIO . encodeFloats . take (fromIntegral $ w * h * 4) $ ps
           t <- emptyTexture min mag
           texImage2DFloat gl_TEXTURE_2D 0
                           (fromIntegral gl_RGBA32F)
                           w h 0
                           gl_RGBA
                           gl_FLOAT
                           arr
           return $ LoadedTexture (fromIntegral w)
                                  (fromIntegral h)
                                  t

emptyTexture :: GLES => (Filter, Maybe Filter) -> Filter -> GL GL.Texture
emptyTexture minf magf = do t <- createTexture
                            bindTexture gl_TEXTURE_2D t
                            param gl_TEXTURE_MIN_FILTER $ mf minf
                            param gl_TEXTURE_MAG_FILTER $ f magf
                            param gl_TEXTURE_WRAP_S gl_REPEAT
                            param gl_TEXTURE_WRAP_T gl_REPEAT
                            return t
        where f Linear = gl_LINEAR
              f Nearest = gl_NEAREST
              mf (Linear, Nothing) = gl_LINEAR
              mf (Linear, Just Nearest) = gl_LINEAR_MIPMAP_NEAREST
              mf (Linear, Just Linear) = gl_LINEAR_MIPMAP_LINEAR
              mf (Nearest, Nothing) = gl_NEAREST
              mf (Nearest, Just Nearest) = gl_NEAREST_MIPMAP_NEAREST
              mf (Nearest, Just Linear) = gl_NEAREST_MIPMAP_LINEAR

              param :: GLES => GLEnum -> GLEnum -> GL ()
              param p v = texParameteri gl_TEXTURE_2D p $ fromIntegral v
