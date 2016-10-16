{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Texture (
        mkTexture,
        mkTextureRaw,
        mkTextureFloat,
        setFilter,
        emptyTexture
) where

import Data.Hashable

import Data.Vect.Float
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
mkTexture w h ps = TextureImage . TexturePixels ps Linear Linear
                                                   (fromIntegral w)
                                                   (fromIntegral h)
                        $ hash (w, h, take (w * h) ps)

mkTextureRaw :: GLES
             => Int         -- ^ Width.
             -> Int         -- ^ Height.
             -> UInt8Array  -- ^ Array of pixel components.
             -> Int         -- ^ Texture hash
             -> Texture
mkTextureRaw w h arr pxhash = TextureImage $ TextureRaw arr Linear Linear
                                                        (fromIntegral w)
                                                        (fromIntegral h)
                                                        $ hash (w, h, pxhash)

-- | Creates a float 'Texture' from a list of vectors.
mkTextureFloat :: GLES
               => Int      -- ^ Width.
               -> Int      -- ^ Height.
               -> [Vec4]   -- ^ List of vectors.
               -> Texture
mkTextureFloat w h vs = TextureImage . TextureFloat ps Linear Linear
                                                       (fromIntegral w)
                                                       (fromIntegral h)
                                $ hash (w, h, take (w * h * 4) ps)
        where ps = vs >>= \(Vec4 x y z w) -> [x, y, z, w]

-- | Change the Texture minifying and magnifying functions. This doesn't work on
-- sublayer textures.
setFilter :: Filter -> Filter -> Texture -> Texture
setFilter min mag (TextureImage (TexturePixels c _ _ w h s)) =
        TextureImage (TexturePixels c min mag w h s)
setFilter min mag (TextureImage (TextureRaw c _ _ w h s)) =
        TextureImage (TextureRaw c min mag w h s)
setFilter min mag (TextureImage (TextureFloat c _ _ w h s)) =
        TextureImage (TextureFloat c min mag w h s)
setFilter _ _ t = t

instance GLES => Resource TextureImage LoadedTexture GL where
        loadResource i = Right <$> loadTextureImage i -- TODO: err check
        unloadResource _ (LoadedTexture _ _ t) = deleteTexture t

loadTextureImage :: GLES => TextureImage -> GL LoadedTexture
loadTextureImage (TexturePixels ps min mag w h hash) =
        do arr <- liftIO . encodeUInt8s . take (fromIntegral $ w * h * 4) $
                        ps >>= \(Color r g b a) -> [r, g, b, a]
           loadTextureImage $ TextureRaw arr min mag w h hash
loadTextureImage (TextureRaw arr min mag w h _) =
        do t <- emptyTexture min mag
           texImage2DUInt gl_TEXTURE_2D 0
                          (fromIntegral gl_RGBA)
                          w h 0
                          gl_RGBA
                          gl_UNSIGNED_BYTE
                          arr
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

emptyTexture :: GLES => Filter -> Filter -> GL GL.Texture
emptyTexture minf magf = do t <- createTexture
                            bindTexture gl_TEXTURE_2D t
                            param gl_TEXTURE_MIN_FILTER $ f minf
                            param gl_TEXTURE_MAG_FILTER $ f magf
                            param gl_TEXTURE_WRAP_S gl_REPEAT
                            param gl_TEXTURE_WRAP_T gl_REPEAT
                            return t
        where f Linear = gl_LINEAR
              f Nearest = gl_NEAREST

              param :: GLES => GLEnum -> GLEnum -> GL ()
              param p v = texParameteri gl_TEXTURE_2D p $ fromIntegral v
