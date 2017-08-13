{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds, KindSignatures,
             TypeFamilies, FlexibleContexts, UndecidableInstances,
             FlexibleInstances, DefaultSignatures, TypeOperators #-}

{-|
Module:      Graphics.Rendering.Ombra.Shader
License:     BSD3
Maintainer:  ziocroc@gmail.com
Stability:   experimental
Portability: GHC only
-}

module Graphics.Rendering.Ombra.Shader (
        module Graphics.Rendering.Ombra.Shader.Language,
        ShaderStage(..),
        Shader(..),
        VertexShader,
        FragmentShader,
        uniform,
        -- * Optimized shaders
        UniformSetter,
        shader,
        shader1,
        uniform',
        (~~),
        (~*),
        -- * Stage-specific shader functionalities
        sample,
        -- ** Derivatives
        dFdx,
        dFdy,
        fwidth,
        -- ** Variables
        -- position,
        -- fragData,
        fragCoord,
        fragFrontFacing,
        -- * Shader types
        MultiShaderType(..),
        ShaderInput(..),
) where

import Control.Arrow
import Control.Applicative
import Control.Category
import Data.Hashable
import Data.MemoTrie
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Graphics.Rendering.Ombra.Internal.GL (GL, UniformLocation)
import Graphics.Rendering.Ombra.Shader.Language
import qualified Graphics.Rendering.Ombra.Shader.Language.Functions as Shader
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Types
import Prelude hiding (id, (.))

newtype UniformSetter x = UniformSetter { unUniformSetter :: x }

instance Functor UniformSetter where
        fmap f (UniformSetter x) = UniformSetter $ f x

instance Applicative UniformSetter where
        pure = UniformSetter
        UniformSetter f <*> UniformSetter x = UniformSetter $ f x

instance Monad UniformSetter where
        return = pure
        UniformSetter x >>= f = f x

hashMST :: MultiShaderType a => a -> a
hashMST = mapMST (fromExpr . HashDummy . hash . toExpr)

-- | Create a shader function that can be reused efficiently.
shader :: (MultiShaderType i, MultiShaderType o) => Shader s i o -> Shader s i o
shader (Shader f hf) = Shader f (memoHash hf)
-- BUG: shader modifies the hash of the shader

uniformSetter :: x -> UniformSetter x
uniformSetter = UniformSetter

-- | 'shader' with an additional parameter that can be used to set the values of
-- the uniforms.
shader1 :: (MultiShaderType i, MultiShaderType o)
        => (Shader s (UniformSetter x, i) o)
        -> (UniformSetter x -> Shader s i o)
shader1 (Shader f hf) = let err = "shader1: not an uniform value"
                            hf' = memoHash $ hf . second ((,) (error err))
                        in \x -> Shader (\(s, i) -> f (s, (x, i))) hf'

memoHash :: (MultiShaderType i, MultiShaderType o)
         => ((UniformID, i) -> (UniformID, o))
         -> ((UniformID, i) -> (UniformID, o))
memoHash hf = let mf = memo $ second hashMST . hf . second fromExprMST
              in mf . second toExprMST

-- | Add a global shader variable that can be set with a CPU value.
uniform :: forall u i o s. BaseUniform u
        => Shader s (u, i) o
        -> Shader s (CPUBase u, i) o
uniform (Shader f hf) = Shader (\((uid, umap), (u, i)) ->
                                let set l = setUniform l (Proxy :: Proxy u) u
                                    ty = typeName (undefined :: u)
                                in f ( (uid + 1, (uid, (ty, set)) : umap)
                                     , (fromExpr $ Uniform uid, i)
                                     )
                               )
                               (\(uid, (_, i)) ->
                                hf (uid + 1, (fromExpr $ Uniform uid, i))
                               )

-- | Like 'uniform' but uses a 'UniformSetter'.
uniform' :: BaseUniform u
         => Shader s (u, i) o
         -> Shader s (UniformSetter (CPUBase u), i) o
uniform' shader = first unUniformSetter ^>> uniform shader

-- | Add a uniform and directly set it with the second operand.
infixl 9 ~~
(~~) :: BaseUniform u => Shader s (u, i) o -> CPUBase u -> Shader s i o
shader ~~ u = const u &&& id ^>> uniform shader

-- | Add a uniform and directly set it with a 'UniformSetter'.
infixl 9 ~*
(~*) :: BaseUniform u
     => Shader s (u, i) o
     -> UniformSetter (CPUBase u)
     -> Shader s i o
shader ~* u = const u &&& id ^>> uniform' shader

-- | Sample a texel from the texture associated with the sampler.
sample :: FragmentShader (GSampler2D, GVec2) GVec4
sample = arr $ uncurry Shader.texture2D

-- | Partial derivative of the argument with respect to the window X coordinate.
dFdx :: GenType a => FragmentShader a a
dFdx = arr Shader.dFdx

-- | Partial derivative of the argument with respect to the window Y coordinate.
dFdy :: GenType a => FragmentShader a a
dFdy = arr Shader.dFdy

-- | Sum of the absolute values of 'dFdx' and 'dFdy'.
fwidth :: GenType a => FragmentShader a a
fwidth = arr Shader.fwidth

-- | The position of the vertex.
position :: VertexShader a GVec4
position = arr $ const Shader.position

-- | The data of the fragment.
fragData :: FragmentShader a (GArray 16 GVec4)
fragData = arr $ const Shader.fragData

-- | The coordinates of the fragment.
fragCoord :: FragmentShader a GVec4
fragCoord = arr $ const Shader.fragCoord

-- | If the fragment belongs to a front-facing primitive.
fragFrontFacing :: FragmentShader a GBool
fragFrontFacing = arr $ const Shader.fragFrontFacing
