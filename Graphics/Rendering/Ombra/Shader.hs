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
        -- * Optimized shaders
        UniformSetter,
        shader,
        shader1,
        uniform',
        (~*),
        sarr,
        -- * Fragment shader functionalities
        -- ** Derivatives
        dFdx,
        dFdy,
        fwidth,
        -- ** Variables
        -- position,
        -- fragData,
        fragCoord,
        fragFrontFacing,
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

-- | 'shader' with an additional parameter that can be used to set the values of
-- the uniforms.
shader1 :: (MultiShaderType i, MultiShaderType o)
        => (Shader s (UniformSetter x, i) o)
        -> (UniformSetter x -> Shader s i o)
shader1 (Shader f hf) = let err = "shader1: not an uniform value"
                            hf' = memoHash $ hf . second ((,) (error err))
                        in \x -> Shader (\(s, i) -> f (s, (x, i))) hf'

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

{-
uniform :: forall u i o s. Uniform u
        => Shader s (u, i) o
        -> Shader s (CPUUniform u, i) o
uniform (Shader f hf) =
        Shader (\((uid, umap), (u, i)) ->
                        let (uniExpr, uid') = buildMST (fromExpr . Uniform) uid
                            acc (proxy :: BaseUniform g => Proxy g)
                                value (uid, umap) =
                                let uvalue = UniformValue proxy value
                                in (uid - 1, (uid, uvalue) : umap)
                            (_, umap') = foldrUniform (Proxy :: Proxy u)
                                                      acc
                                                      (uid' - 1, umap)
                                                      u
                        in f ((uid', umap'), (uniExpr, i))
               )
               (\(uid, (_, i)) ->
                       let (uniExpr, uid') = buildMST (fromExpr . Uniform) uid
                       in hf (uid', (uniExpr, i))
               )

textureUniform :: Shader s (Sampler2D i) o -> Shader s (Texture, i) o
textureUniform (Shader f hf) =
        Shader (\(ShaderState uid umap tmap, (tex, i)) ->
                f ( ShaderState (uid + 1)
                                ((uid, UniformTexture tex) : umap)
                                ((uid, tex) : tmap)
                  , (fromExpr . Uniform $ uid, i)
                  )
               )
               (\(uid, (_, i)) -> hf (uid + 1, (fromExpr . Uniform $ uid, i)))

textureUniform' :: Shader s (Sampler2D i) o
                -> Shader s (UniformSetter Texture, i) o
textureUniform'

-- | Like 'uniform' but uses a 'UniformSetter'.
uniform' :: Uniform u
         => Shader s (u, i) o
         -> Shader s (UniformSetter (CPUUniform u), i) o
uniform' shader = first unUniformSetter ^>> uniform shader

-- | Add a uniform and directly set it with the second operand.
infixl 9 ~~
(~~) :: Uniform u => Shader s (u, i) o -> CPUUniform u -> Shader s i o
shader ~~ u = const u &&& id ^>> uniform shader

-- | Add a uniform and directly set it with a 'UniformSetter'.
infixl 9 ~*
(~*) :: Uniform u
     => Shader s (u, i) o
     -> UniformSetter (CPUUniform u)
     -> Shader s i o
shader ~* u = const u &&& id ^>> uniform' shader
-}

{-
-- | Sample a texel from a texture.
sample :: FragmentShader (Texture, GVec2) GVec4
sample = arr $ uncurry Shader.texture2D
-}

{-
samplerGBuffer :: FragmentShaderOutput o
               -> FragmentShader (GBuffer o OutUsed t) (GVec2 -> o)

samplerGBuffer' :: FragmentShaderOutput o
                -> FragmentShader (UniformSetter (GBuffer o OutUsed t))
                                  (GVec2 -> o)
-}

-- | Partial derivative of the argument with respect to the window X coordinate.
dFdx :: GenType a => FragmentShader a a
dFdx = arr Shader.dFdx

-- | Partial derivative of the argument with respect to the window Y coordinate.
dFdy :: GenType a => FragmentShader a a
dFdy = arr Shader.dFdy

-- | Sum of the absolute values of 'dFdx' and 'dFdy'.
fwidth :: GenType a => FragmentShader a a
fwidth = arr Shader.fwidth

{-
-- | The position of the vertex.
position :: VertexShader a GVec4
position = arr $ const Shader.position

-- | The data of the fragment.
fragData :: FragmentShader a (GArray 16 GVec4)
fragData = arr $ const Shader.fragData
-}

-- | The coordinates of the fragment.
fragCoord :: FragmentShader a GVec4
fragCoord = arr $ const Shader.fragCoord

-- | If the fragment belongs to a front-facing primitive.
fragFrontFacing :: FragmentShader a GBool
fragFrontFacing = arr $ const Shader.fragFrontFacing
