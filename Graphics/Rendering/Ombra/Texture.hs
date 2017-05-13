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
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture)
import Graphics.Rendering.Ombra.Texture.Internal
import Graphics.Rendering.Ombra.Texture.Types
import Graphics.Rendering.Ombra.Vector

-- | Creates a 'Texture' from a list of pixels.
mkTexture :: GLES
          => Int        -- ^ Width. Must be a power of two.
          -> Int        -- ^ Height. Must be a power of two.
          -> Bool       -- ^ Generate mipmaps automatically.
          -> [[Color]]  -- ^ List of pixels, one for each level of detail.
                        -- The first element is the base image level, the second
                        -- image is half the size of the base, and so on.
                        -- Use just one level of detail if you don't want
                        -- mipmaps, or you used True in the previous argument.
          -> Texture
mkTexture w h g pss = TextureImage . TexturePixels g pss minfilter Linear
                                                   (fromIntegral w)
                                                   (fromIntegral h)
                        -- TODO: hash based on the mipmaps too
                           $ hash (w, h, length pss, g, take (w * h) (head pss))
        where minfilter | g = (Linear, Just Nearest)
                        | (_:_:_) <- pss = (Linear, Just Nearest)
                        | otherwise = (Linear, Nothing)

mkTextureRaw :: GLES
             => Int             -- ^ Width.
             -> Int             -- ^ Height.
             -> Bool            -- ^ Generate mipmaps.
             -> [UInt8Array]    -- ^ Array of pixel components, one for each
                                -- level of detail.
             -> Int             -- ^ Texture hash
             -> Texture
mkTextureRaw w h g arr pxhash = TextureImage $ TextureRaw g arr minfilter Linear
                                                          (fromIntegral w)
                                                          (fromIntegral h)
                                                          $ hash (w, h, pxhash)
        where minfilter | g = (Linear, Just Nearest)
                        | (_:_:_) <- arr = (Linear, Just Nearest)
                        | otherwise = (Linear, Nothing)

-- | Creates a float 'Texture' from a list of vectors.
mkTextureFloat :: GLES
               => Int      -- ^ Width.
               -> Int      -- ^ Height.
               -> [Vec4]   -- ^ List of vectors.
               -> Texture
mkTextureFloat w h vs = TextureImage . TextureFloat ps (Linear, Nothing) Linear
                                                       (fromIntegral w)
                                                       (fromIntegral h)
                                $ hash (w, h, take (w * h * 4) ps)
        where ps = vs >>= \(Vec4 x y z w) -> [x, y, z, w]

-- | Change the Texture minifying and magnifying functions. This doesn't work on
-- sublayer textures.
setFilter :: (Filter, Maybe Filter)     -- ^ Minification filter and mipmap
                                        -- filter.
          -> Filter                     -- ^ Magnification filter.
          -> Texture
          -> Texture
setFilter min mag (TextureImage (TexturePixels g c _ _ w h s)) =
        TextureImage (TexturePixels g c min mag w h s)
setFilter min mag (TextureImage (TextureRaw g c _ _ w h s)) =
        TextureImage (TextureRaw g c min mag w h s)
setFilter min mag (TextureImage (TextureFloat c _ _ w h s)) =
        TextureImage (TextureFloat c min mag w h s)
setFilter _ _ t = t

-- | Generate a 1x1 texture.
colorTex :: GLES => Color -> Texture
colorTex c = mkTexture 1 1 False [[c]]
