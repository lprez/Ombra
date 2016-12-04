module Graphics.Rendering.Ombra.Texture (
        Texture,
        mkTexture,
        mkTextureFloat,
        mkTextureRaw,
        Filter(..),
        setFilter,
        colorTex
) where

import Data.Hashable
import Data.Vect.Float
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture)
import Graphics.Rendering.Ombra.Texture.Internal

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

-- | Generate a 1x1 texture.
colorTex :: GLES => Color -> Texture
colorTex c = mkTexture 1 1 [ c ]
