{-# LANGUAGE GADTs, RankNTypes, DataKinds, KindSignatures #-}

module Graphics.Rendering.Ombra.Layer.Types where

import Data.Word (Word8)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Object.Types
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Texture.Types
import Graphics.Rendering.Ombra.Texture.Internal

-- | An 'Object' associated with a program.
type Layer = Layer' Drawable () ()

-- | A layer with a return value. It may also be 'NonDrawable', this means that
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
        TextureLayer :: Bool                    -- Use drawBuffers
                     -> [LayerType]             -- Attachments
                     -> (Int, Int)              -- Width, height
                     -> (Int, Int, Int, Int)    -- Inspect rectangle
                     -> Bool                    -- Inspect color
                     -> Bool                    -- Inspect depth
                     -> Layer' s t a            -- Layer to draw
                     -> Layer' NonDrawable t
                               (a, [TTexture t], Maybe [Color], Maybe [Word8])
        Permanent :: TTexture t -> Layer' NonDrawable t Texture
        WithTTextures :: [TTexture t]
                      -> ([Texture] -> Layer)
                      -> Layer' NonDrawable t ()
        Free :: (forall t. Layer' NonDrawable t a) -> Layer' s t a
        Clear :: [Buffer] -> Layer' s t ()
        Cast :: Layer' Drawable t a -> Layer' Drawable t' a
        Bind :: Layer' s t a -> (a -> Layer' s t b) -> Layer' s t b
        Return :: a -> Layer' s t a

-- | Temporary texture.
newtype TTexture t = TTexture LoadedTexture deriving Eq

data LayerStatus = Drawable | NonDrawable

data Buffer = ColorBuffer | DepthBuffer | StencilBuffer

data LayerType = ColorLayer
               | DepthLayer
               | DepthStencilLayer
               | BufferLayer Int deriving Eq

instance Functor (Layer' s t) where
        fmap f = flip Bind $ Return . f

instance Applicative (Layer' s t) where
        lf <*> lx = Bind lf $ \f -> Bind lx $ \x -> Return $ f x
        pure = Return

instance Monad (Layer' s t) where
        (>>=) = Bind
        return = Return

