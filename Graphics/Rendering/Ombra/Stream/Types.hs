{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, DataKinds #-}

module Graphics.Rendering.Ombra.Stream.Types where

import Control.Arrow
import qualified Data.IntMap as M
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Shader

class MapShader f s | f -> s where
        mapShader :: Shader s i o -> f i -> f o

data VertexStream v = forall i. (GeometryVertex i, ShaderInput i) =>
                                VertexStream (VertexShader i v) (Geometry i)

data FragmentStream f = forall v. ShaderInput v =>
                                  FragmentStream (FragmentShader v f)
                                                 (VertexStream (GVec4, v))

instance Functor VertexStream where
        fmap f (VertexStream shader g) = VertexStream (shader >>^ f) g

instance Functor FragmentStream where
        fmap f (FragmentStream shader v) = FragmentStream (shader >>^ f) v

instance MapShader VertexStream VertexShaderStage where
        mapShader f (VertexStream shader g) = VertexStream (shader >>> f) g

instance MapShader FragmentStream FragmentShaderStage where
        mapShader f (FragmentStream shader v) = FragmentStream (shader >>> f) v
