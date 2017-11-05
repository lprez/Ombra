-- |
-- Module:      Graphics.Rendering.Ombra.Texture
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: portable

module Graphics.Rendering.Ombra.Texture (
        MonadTexture,
        Texture,
        mkTexture,
        mkTextureFloat,
        mkTextureRaw,
        colorTex,
        -- * Parameters
        TextureParameters,
        Filter(..),
        WrappingFunction(..),
        parameters,
        potParameters,
        potLinear
) where

import Data.Hashable
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture)
import Graphics.Rendering.Ombra.Texture.Draw
import Graphics.Rendering.Ombra.Texture.Types
import Graphics.Rendering.Ombra.Vector

-- | Create a 'TextureParameters'.
parameters :: Filter                    -- ^ Minification filter
           -> Filter                    -- ^ Magnification filter
           -> TextureParameters
parameters min mag = potParameters (min, Nothing) mag
                                   False
                                   ClampToEdge ClampToEdge

-- | This function provides more features than 'parameters', but, on WebGL, the
-- resulting 'TextureParameters' will not work with textures whose width or
-- height is not a power of two.
potParameters :: (Filter, Maybe Filter) -- ^ Minification filter.
              -> Filter                 -- ^ Magnification filter.
              -> Bool                   -- ^ Generate mipmaps automatically. Do
                                        -- not use mipmaps with 'GBuffer's or
                                        -- 'DepthBuffer's.
              -> WrappingFunction       -- ^ Horizontal wrapping function.
              -> WrappingFunction       -- ^ Vertical wrapping function.
              -> TextureParameters
potParameters = TextureParameters

-- | 'potParameters' with linear filters and repeat.
potLinear :: Bool               -- ^ Generate mipmaps
          -> TextureParameters
potLinear g = potParameters (Linear, mipf) Linear g Repeat Repeat
        where mipf = if g then Just Nearest else Nothing

-- | Creates a 'Texture' from a list of pixels.
mkTexture :: GLES
          => Int        -- ^ Width.
          -> Int        -- ^ Height.
          -> TextureParameters
          -> [[Color]]  -- ^ List of pixels, one for each level of detail.
                        -- The first element is the base image level, the second
                        -- image is half the size of the base, and so on.
                        -- Use just one level of detail if you don't want
                        -- mipmaps, or you used True in the previous argument.
          -> Texture
mkTexture w h params pss =
        TextureImage $ TexturePixels pss
                                     params
                                     (fromIntegral w)
                                     (fromIntegral h)
                                     (hash ( w, h, params
                                           -- XXX
                                           , length pss
                                           , take (w * h) (head pss)
                                           )
                                     )

mkTextureRaw :: GLES
             => Int             -- ^ Width.
             -> Int             -- ^ Height.
             -> TextureParameters
             -> [UInt8Array]    -- ^ Array of pixel components, one for each
                                -- level of detail.
             -> Int             -- ^ Hash of the arrays
             -> Texture
mkTextureRaw w h params arr pxhash =
        TextureImage $ TextureRaw arr
                                  params
                                  (fromIntegral w)
                                  (fromIntegral h)
                                  (hash (w, h, params, pxhash))

-- | Creates a float 'Texture' from a list of vectors.
mkTextureFloat :: GLES
               => Int      -- ^ Width.
               -> Int      -- ^ Height.
               -> TextureParameters
               -> [Vec4]   -- ^ List of vectors.
               -> Texture
mkTextureFloat w h params vs =
        TextureImage $ TextureFloat ps
                                    params
                                    (fromIntegral w)
                                    (fromIntegral h)
                                    (hash (w, h, params, take (w * h * 4) ps))
        where ps = vs >>= \(Vec4 x y z w) -> [x, y, z, w]

-- | Generate a 1x1 texture.
colorTex :: GLES => Color -> Texture
colorTex c = mkTexture 1 1 (potLinear False) [[c]]
