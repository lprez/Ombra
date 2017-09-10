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
             
data TextureImage = TexturePixels [[Color]] TextureParameters GLSize GLSize Int
                  | TextureRaw [UInt8Array] TextureParameters GLSize GLSize Int
                  | TextureFloat [Float] TextureParameters GLSize GLSize Int

data TextureParameters = TextureParameters
        { minificationFilter :: (Filter, Maybe Filter)
        , magnificationFilter :: Filter
        , generateMipmaps :: Bool
        , wrapS :: WrappingFunction
        , wrapT :: WrappingFunction
        }
        deriving Eq

data WrappingFunction = Repeat | MirroredRepeat | ClampToEdge deriving Eq

data Filter = Linear    -- ^ Average of the four nearest pixels.
            | Nearest   -- ^ Nearest pixel.
            deriving Eq

data LoadedTexture = LoadedTexture GLSize GLSize Int GL.Texture

instance Hashable WrappingFunction where
        hashWithSalt s Repeat = hashWithSalt s (1 :: Int)
        hashWithSalt s MirroredRepeat = hashWithSalt s (2 :: Int)
        hashWithSalt s ClampToEdge = hashWithSalt s (3 :: Int)

instance Hashable Filter where
        hashWithSalt s Linear = hashWithSalt s True
        hashWithSalt s Nearest = hashWithSalt s False

instance Hashable TextureParameters where
        hashWithSalt s (TextureParameters f1 f2 m w1 w2) =
                hashWithSalt s (f1, f2, m, w1, w2)

instance Hashable TextureImage where
        hashWithSalt salt tex = hashWithSalt salt $ textureHash tex

instance Eq TextureImage where
        (TexturePixels _ _ _ _ h) == (TexturePixels _ _ _ _ h') = h == h'
        (TextureRaw _ _ _ _ h) == (TextureRaw _ _ _ _ h') = h == h'
        (TextureFloat _ _ _ _ h) == (TextureFloat _ _ _ _ h') = h == h'
        _ == _ = False

instance GLES => Eq LoadedTexture where
        LoadedTexture _ _ _ t == LoadedTexture _ _ _ t' = t == t'

textureHash :: TextureImage -> Int
textureHash (TexturePixels _ _ _ _ h) = h
textureHash (TextureRaw _ _ _ _ h) = h
textureHash (TextureFloat _ _ _ _ h) = h
