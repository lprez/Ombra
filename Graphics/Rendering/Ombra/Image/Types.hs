{-# LANGUAGE GADTs, DataKinds, MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Image.Types where

import Data.Foldable
import Data.Semigroup
import Control.Arrow
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Shader

data Image o where
       Image :: (ShaderInput i, ShaderInput v, Foldable t, GeometryVertex i)
             => t (Geometry i, vu, fu)
             -> (UniformSetter vu -> VertexShader i (GVec4, v))
             -> (UniformSetter fu -> FragmentShader v o)
             -> Image o
       SeqImage :: Image o -> Image o -> Image o

instance Functor Image where
        fmap f (Image g vs fs) = Image g vs (\fu -> fs fu >>^ f)

instance MapShader Image FragmentShaderStage where
        mapShader f (Image g vs fs) = Image g vs (\fu -> fs fu >>> f)

instance Semigroup (Image o) where
        (<>) = SeqImage
