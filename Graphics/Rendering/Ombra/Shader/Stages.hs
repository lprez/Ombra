{-# LANGUAGE TypeOperators, DataKinds, GeneralizedNewtypeDeriving,
             DeriveDataTypeable, RankNTypes, FlexibleContexts,
             TypeFamilies, ConstraintKinds #-}

module Graphics.Rendering.Ombra.Shader.Stages (
        VertexShader,
        ValidVertex,
        FragmentShader,
        VertexShaderOutput(Vertex),
        FragmentShaderOutput(Fragment)
) where

import Data.Typeable

import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.Shader

-- | A 'Shader' with a 'VertexShaderOutput' output.
type VertexShader g i o = Shader g i (VertexShaderOutput ': o)

-- | A 'Shader' with only a 'FragmentShaderOutput' output.
type FragmentShader g i = Shader g i (FragmentShaderOutput ': '[])

-- | The condition for a valid 'VertexShader'.
type ValidVertex g i o = (Valid g i o, IsMember VertexShaderOutput o ~ False)

-- | The position of the vertex.
newtype VertexShaderOutput = Vertex Vec4 deriving (Typeable, ShaderType)

-- | The RGBA color of the fragment (1.0 = #FF).
newtype FragmentShaderOutput = Fragment Vec4 deriving (Typeable, ShaderType)
