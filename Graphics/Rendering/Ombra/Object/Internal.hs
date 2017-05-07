{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators,
             FlexibleContexts #-}

module Graphics.Rendering.Ombra.Object.Internal where

import Data.Proxy (Proxy)
import Data.Monoid
import qualified Graphics.Rendering.Ombra.Blend as Blend
import qualified Graphics.Rendering.Ombra.Stencil as Stencil
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL (ActiveTexture)
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.ShaderVar
import Graphics.Rendering.Ombra.Texture

-- | A geometry associated with some uniforms.
data Object (gs :: [*]) (is :: [*]) where
        -- | Add a Global to an Object.
        (:~>) :: Global g -> Object gs is -> Object (g ': gs) is
        Mesh :: ((i : is') ~ is) => Geometry (i : is') -> Object '[] is
        NoMesh :: Object gs is
        Prop :: ObjProp -> Object gs is -> Object gs is
        Append :: Object gs is -> Object gs is -> Object gs is

data ObjProp = Blend (Maybe Blend.Mode)
             | Stencil (Maybe Stencil.Mode)
             | Cull (Maybe CullFace)
             | DepthTest Bool
             | DepthMask Bool
             | ColorMask (Bool, Bool, Bool, Bool)

infixr 2 :~>

-- | The value of a GPU uniform.
data Global g where
        Single :: (ShaderVar g, Uniform 'S g)
               => (a -> g) -> (CPU 'S g) -> Global g
        Mirror :: (ShaderVar g, Uniform 'M g)
               => Proxy g -> (CPU 'M g) -> Global g
        WithTexture :: Texture -> (ActiveTexture -> Global g) -> Global g
        WithTextureSize :: Texture -> ((Int, Int) -> Global g) -> Global g
        WithFramebufferSize :: ((Int, Int) -> Global g) -> Global g


-- Side(s) to be culled.
data CullFace = CullFront | CullBack | CullFrontBack deriving Eq

-- TODO: should be Semigroup, mempty is unsafe
instance (ShaderVars gs, ShaderVars is) => Monoid (Object gs is) where
        mempty = NoMesh
        mappend = Append
