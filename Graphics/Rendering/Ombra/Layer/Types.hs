{-# LANGUAGE GADTs, RankNTypes, DataKinds, KindSignatures #-}

module Graphics.Rendering.Ombra.Layer.Types where

import Data.Word (Word8, Word16)
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Object.Types
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Texture.Types
import Graphics.Rendering.Ombra.Texture.Internal
import Graphics.Rendering.Ombra.Vector

-- | An 'Object' associated with a program.
type Layer = Layer' Drawable () ()

-- | A layer with a return value. It can be 'NonDrawable', this means that
-- there are some protected temporary resources and you have to call 'drawable'
-- to turn it into a normal layer. The second parameter prevents the 'TTexture's
-- from being returned by a @NonDrawable@ layer in a @drawable@ operation.
--
-- Note that layers are monads: @flip ('>>')@ is equivalent to 'over' for
-- Drawable layers, while ('>>='), in combination with the *ToTexture functions,
-- can be used to achieve the same effect of the subLayer functions.
data Layer' (s :: LayerStatus) t a where
        Layer :: (Subset pi oi, Subset pg og)
              => Program pg pi
              -> Object og oi
              -> Layer' s t ()
        NewTTexture :: Int
                    -> Int
                    -> Filter
                    -> Filter
                    -> TTextureType bufs
                    -> Layer' NonDrawable t (TTexture bufs t)
        WithTTextures :: [TTexture bufs t]
                      -> ([Texture] -> Layer' s t ())
                      -> Layer' NonDrawable t ()
        Permanent :: TTexture bufs t -> Layer' NonDrawable t Texture
        TextureLayer :: Int
                     -> Int
                     -> [TTexture '[ColorBuffer] t]
                     -> Either (TTexture '[DepthBuffer] t)
                               (TTexture '[DepthBuffer, StencilBuffer] t)
                     -> Layer' s t a
                     -> Layer' NonDrawable t a
        ReadColor :: (Int, Int, Int, Int) -> Layer' s t [Color]
        ReadColorFloat :: (Int, Int, Int, Int) -> Layer' s t [Vec4]
        ReadDepth :: (Int, Int, Int, Int) -> Layer' s t [Word16]
        ReadDepthFloat :: (Int, Int, Int, Int) -> Layer' s t [Float]
        ReadStencil :: (Int, Int, Int, Int) -> Layer' s t [Word8]
        Free :: (forall t. Layer' NonDrawable t a) -> Layer' s t a
        Clear :: [Buffer] -> Layer' s t ()
        Cast :: Layer' Drawable t a -> Layer' s t' a
        Bind :: Layer' s t a -> (a -> Layer' s t b) -> Layer' s t b
        Return :: a -> Layer' s t a

-- | Target texture. This is created by functions like 'subLayer' and
-- 'depthToTexture', and it's only temporary, unless you use the 'permanent'
-- function.
data TTexture (bufs :: [Buffer]) t = TTexture (TTextureType bufs) LoadedTexture

data TTextureType (bufs :: [Buffer]) where
        RGBAFloatTTexture :: TTextureType '[ColorBuffer]
        RGBAByteTTexture :: TTextureType '[ColorBuffer]
        ShortTTexture :: TTextureType '[DepthBuffer]
        IntTTexture :: TTextureType '[DepthBuffer, StencilBuffer]

data LayerStatus = Drawable | NonDrawable

data Buffer = ColorBuffer | DepthBuffer | StencilBuffer

instance Functor (Layer' s t) where
        fmap f = flip Bind $ Return . f

instance Applicative (Layer' s t) where
        lf <*> lx = Bind lf $ \f -> Bind lx $ \x -> Return $ f x
        pure = Return

instance Monad (Layer' s t) where
        (>>=) = Bind
        return = Return

instance GLES => Eq (TTexture bufs t) where
        TTexture _ lt == TTexture _ lt' = lt == lt'
