{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Texture.Types where

import Data.Hashable
import Graphics.Rendering.Ombra.Backend hiding (Texture)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Color

-- | A texture.
data Texture = TextureImage TextureImage
             | TextureLoaded LoadedTexture
             deriving (Eq, Ord)
             
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

instance Ord TextureImage where
        compare (TexturePixels _ _ _ _ h) (TexturePixels _ _ _ _ h') =
                compare h h'
        compare (TexturePixels _ _ _ _ _) _ = LT
        compare _ (TexturePixels _ _ _ _ _) = GT

        compare (TextureRaw _ _ _ _ h) (TextureRaw _ _ _ _ h') =
                compare h h'
        compare (TextureRaw _ _ _ _ _) _ = LT
        compare _ (TextureRaw _ _ _ _ _) = GT

        compare (TextureFloat _ _ _ _ h) (TextureFloat _ _ _ _ h') =
                compare h h'

instance GLES => Eq LoadedTexture where
        LoadedTexture _ _ _ t == LoadedTexture _ _ _ t' = t == t'

instance GLES => Ord LoadedTexture where
        compare (LoadedTexture _ _ _ t) (LoadedTexture _ _ _ t') = compare t t'

instance GLES => Hashable LoadedTexture where
        hashWithSalt salt (LoadedTexture _ _ _ t) = hashWithSalt salt t

instance GLES => Hashable Texture where
        hashWithSalt salt tex =
                let etex = case tex of
                                TextureImage t -> Left t
                                TextureLoaded (LoadedTexture _ _ _ t) -> Right t
                in hashWithSalt salt etex

textureHash :: TextureImage -> Int
textureHash (TexturePixels _ _ _ _ h) = h
textureHash (TextureRaw _ _ _ _ h) = h
textureHash (TextureFloat _ _ _ _ h) = h
