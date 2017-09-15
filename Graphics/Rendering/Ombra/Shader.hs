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
        Shader,
        VertexShader,
        FragmentShader,
        -- * Uniforms
        uniform,
        (~~),
        foldUniforms,
        -- * Optimized shaders
        UniformSetter,
        shader,
        sarr,
        shaderParam,
        pshader,
        ushader,
        pushader,
        uniform',
        (~*),
        foldUniforms',
        -- * Fragment shader functionalities
        Fragment(..),
        farr,
        fragment,
        -- * Loops
        forLoop,
        foldGArray,
        -- * Classes
        MultiShaderType(..),
        ShaderInput(..),
        FragmentShaderOutput(..),
        MapShader(..),
        Uniform(..)
) where

import Control.Arrow
import Control.Applicative
import Control.Category
import Data.Hashable
import Data.MemoTrie
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Internal.GL (Sampler2D)
import Graphics.Rendering.Ombra.Shader.Language
import qualified Graphics.Rendering.Ombra.Shader.Language.Functions as Shader
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Texture (Texture)
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

-- | This variant of 'shader' can be used with shaders that have a mostly static
-- parameter. It will create a different shader every time the parameter changes
-- to a new value, therefore parameters should not be used for things like
-- model matrices (for which uniforms are more appropriate). Unlike uniforms,
-- parameters can be used anywhere, in particular they can be used to change the
-- shader structure.
shaderParam :: (HasTrie p, MultiShaderType i, MultiShaderType o)
            => Shader s (p, i) o
            -> Shader s (p, i) o
shaderParam (Shader f hf) =
        let hf' = memo (\p -> memoHash $ \(uid, i) -> hf (uid, (p, i)))
        in Shader f (\(uid, (p, i)) -> hf' p (uid, i))

-- | See 'shaderParam'.
pshader :: (HasTrie p, MultiShaderType i, MultiShaderType o)
        => (p -> Shader s i o)
        -> (p -> Shader s i o)
pshader shaderf = let shader' = shaderParam $ first shaderf ^>> app
                  in \p -> const p &&& id ^>> shader'

-- | 'shader' with an additional parameter that can be used to set the values of
-- the uniforms.
ushader :: (MultiShaderType i, MultiShaderType o)
        => (UniformSetter x -> Shader s i o)
        -> (UniformSetter x -> Shader s i o)
ushader shaderf = let err = "ushader: not an uniform value"
                      Shader _ hf = shaderf $ error err
                      hf' = memoHash hf
                  in \x -> let Shader f _ = shaderf x in Shader f hf'

-- | Combination of 'pshader' and 'ushader'.
pushader :: (HasTrie p, MultiShaderType i, MultiShaderType o)
         => (p -> UniformSetter x -> Shader s i o)
         -> (p -> UniformSetter x -> Shader s i o)
pushader shaderf = let err = error "pushader: not an uniform value"
                       hf' = memo $ \p -> let Shader _ hf = shaderf p err
                                          in memoHash hf
                   in \p x -> let Shader f _ = shaderf p x
                              in Shader f $ hf' p

-- | @'shader' . 'arr'@
sarr :: (MultiShaderType i, MultiShaderType o) => (i -> o) -> Shader s i o
sarr = shader . arr

memoHash :: (MultiShaderType i, MultiShaderType o)
         => ((UniformID, i) -> (UniformID, o))
         -> ((UniformID, i) -> (UniformID, o))
memoHash hf = let mf = memo $ second hashMST . hf . second fromExprMST
              in mf . second toExprMST

-- | Add a shader variable that can be set with a CPU value.
uniform :: forall u s. Uniform u => Shader s (CPUUniform u) u
uniform = Shader (\(ShaderState uid umap tmap, multiValue) ->
                        let (uniExpr, uid') =
                                buildMST' (\t -> fromExpr . Uniform t) uid
                            acc value@(UniformValue _ _) (uid, umap, tmap) =
                                    (uid - 1, (uid, value) : umap, tmap)
                            acc value@(UniformTexture tex) (uid, umap, tmap) =
                                    (uid - 1, (uid, value) : umap, tex : tmap)
                            (_, umap', tmap') =
                                    foldrUniform (Proxy :: Proxy u) acc
                                                 (uid' - 1, umap, tmap)
                                                 multiValue
                        in (ShaderState uid' umap' tmap', uniExpr)
                 )
                 (\(uid, _) ->
                       let (uniExpr, uid') =
                               buildMST' (\t -> fromExpr . Uniform t) uid
                       in (uid', uniExpr)
                 )

-- | Like 'uniform' but uses a 'UniformSetter'.
uniform' :: Uniform u => Shader s (UniformSetter (CPUUniform u)) u
uniform' = unUniformSetter ^>> uniform

-- | Add a uniform and directly set it with the second operand.
infixl 9 ~~
(~~) :: Uniform u => Shader s (u, i) o -> CPUUniform u -> Shader s i o
shader ~~ u = (const u ^>> uniform) &&& id >>> shader

-- | Add a uniform and directly set it with a 'UniformSetter'.
infixl 9 ~*
(~*) :: Uniform u
     => Shader s (u, i) o
     -> UniformSetter (CPUUniform u)
     -> Shader s i o
shader ~* u = (const u ^>> uniform') &&& id >>> shader

-- | This works like 'sarr' but provides a 'Fragment'.
farr :: (MultiShaderType i, MultiShaderType o)
     => (Fragment -> i -> o)
     -> FragmentShader i o
farr f = shader $ arr (f frag)

fragment :: FragmentShader a Fragment
fragment = arr $ const frag

frag :: Fragment
frag = Fragment { fragCoord = Shader.fragCoord
                , fragFrontFacing = Shader.fragFrontFacing
                , dFdx = Shader.dFdx
                , dFdy = Shader.dFdy
                , fwidth = Shader.fwidth
                }

-- | This function implements raw GLSL loops. The same effect can be achieved
-- using Haskell list functions, but that may result in a large compiled GLSL
-- source, which in turn might slow down compilation or cause an out of memory
-- error.
forLoop :: ShaderInput a 
        => Int -- ^ Maximum number of iterations (should be as low as possible)
        -> a -- ^ Initial value
        -> (GInt -> a -> (a, GBool)) -- ^ Iteration -> Old value -> (Next, Stop)
        -> a
forLoop iters iacc f = buildFromExprList $
        Shader.unsafeLoop (fromExpr . Literal "int" $ show iters)
                          (foldrMST (\x -> ((typeName x, toExpr x) :)) [] iacc)
                          (\i es -> let acc = buildFromExprList es
                                        (acc', stop) = f i acc
                                        es' = foldrMST (\x -> (toExpr x :))
                                                       [] acc'
                                    in (es', stop))
        -- XXX
        where buildFromExprList es = fst $ buildMST (\i -> fromExpr $ es !! i) 0

foldGArray :: forall t n a. (ShaderType t, KnownNat n, ShaderInput a)
           => (a -> t -> a)
           -> a
           -> GArray n t
           -> a
foldGArray f iacc arr = forLoop (fromIntegral $ natVal (Proxy :: Proxy n))
                                iacc
                                (\i acc -> (f acc $ arr ! i, false))

foldUniforms :: forall a u s. (ShaderInput a, ArrayUniform u, GLES)
             => Shader s (((a -> u -> a), a), [CPUBase u]) a
foldUniforms = (\((f, i), us) -> case someNatVal . fromIntegral $ length us of
                                      Just (SomeNat p) -> (foldArray p f i, us)
               ) ^>> app
        where foldArray :: forall n. KnownNat n
                        => Proxy n
                        -> (a -> u -> a)
                        -> a
                        -> Shader s [CPUBase u] a
              foldArray p f i = baseUniformGArray p (Proxy :: Proxy u) $
                                        uniform >>^ \(arr :: GArray n u) ->
                                                        foldGArray f i arr

foldUniforms' :: (ShaderInput a, ArrayUniform u, GLES)
              => Shader s (((a -> u -> a), a), UniformSetter [CPUBase u]) a
foldUniforms' = second unUniformSetter ^>> foldUniforms
