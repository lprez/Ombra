{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Texture.Types where

import Data.Hashable
import Graphics.Rendering.Ombra.Backend hiding (Texture)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Color

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
