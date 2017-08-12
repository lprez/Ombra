{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds, KindSignatures,
             TypeFamilies, FlexibleContexts, UndecidableInstances,
             FlexibleInstances #-}

{-|
Module:      Graphics.Rendering.Ombra.Shader
License:     BSD3
Maintainer:  ziocroc@gmail.com
Stability:   experimental
Portability: GHC only
-}

module Graphics.Rendering.Ombra.Shader (
        module Graphics.Rendering.Ombra.Shader.Language,
        MultiShaderType(..),
        ShaderInput(..),
        ShaderStage(..),
        Shader(..),
        VertexShader,
        FragmentShader,
        uniform,
        (~~),
        -- * Optimized shaders
        UniformSetter(..),
        uniformSetter,
        shader,
        shader1,
        uniform',
        withUniformSetter,
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
        -- *
        UniformID,
        uniformList
) where

import Control.Arrow
import Control.Category
import Data.Hashable
import Data.MemoTrie
import Data.Proxy
import GHC.TypeLits
import Graphics.Rendering.Ombra.Internal.GL (GL, UniformLocation)
import Graphics.Rendering.Ombra.Shader.Language
import qualified Graphics.Rendering.Ombra.Shader.Language.Functions as Shader
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.CPU
import Prelude hiding (id, (.))

-- | A type that contains a finite amount of 'ShaderType's.
class MultiShaderType a => ShaderInput a where
        buildMST :: (forall x. ShaderType x => Int -> x) -> Int -> (a, Int)

-- si potrebbe evitare di dover istanziare HasTrie per ogni MST utilizzando Reg
-- come ExprMST

-- | A type that contains zero or more 'ShaderType's.
class HasTrie (ExprMST a) => MultiShaderType a where
        type ExprMST a
        mapMST :: (forall x. ShaderType x => x -> x) -> a -> a
        foldrMST :: (forall x. ShaderType x => x -> b -> b) -> b -> a -> b
        toExprMST :: a -> ExprMST a
        fromExprMST :: ExprMST a -> a

instance MultiShaderType GBool where
        type ExprMST GBool = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBool where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GFloat where
        type ExprMST GFloat = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GFloat where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GInt where
        type ExprMST GInt = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GInt where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GSampler2D where
        type ExprMST GSampler2D = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GSampler2D where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GVec2 where
        type ExprMST GVec2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec2 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GVec3 where
        type ExprMST GVec3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec3 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GVec4 where
        type ExprMST GVec4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec4 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GIVec2 where
        type ExprMST GIVec2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec2 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GIVec3 where
        type ExprMST GIVec3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec3 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GIVec4 where
        type ExprMST GIVec4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec4 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GBVec2 where
        type ExprMST GBVec2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec2 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GBVec3 where
        type ExprMST GBVec3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec3 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GBVec4 where
        type ExprMST GBVec4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec4 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GMat2 where
        type ExprMST GMat2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat2 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GMat3 where
        type ExprMST GMat3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat3 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType GMat4 where
        type ExprMST GMat4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat4 where
        buildMST f i = (f i, i + 1)

instance MultiShaderType () where
        type ExprMST () = ()
        mapMST _ = id
        foldrMST _ x _ = x
        toExprMST = id
        fromExprMST = id

instance (KnownNat n, ShaderType t) => MultiShaderType (GArray n t) where
        type ExprMST (GArray n t) = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance (KnownNat n, ShaderType t) => ShaderInput (GArray n t) where
        buildMST f i = (f i, i + 1)

instance (MultiShaderType a, MultiShaderType b) => MultiShaderType (a, b) where
        type ExprMST (x, y) = (ExprMST x, ExprMST y)
        mapMST f (x, y) = (mapMST f x, mapMST f y)
        foldrMST f s (x, y) = foldrMST f (foldrMST f s y) x
        toExprMST (x, y) = (toExprMST x, toExprMST y)
        fromExprMST (x, y) = (fromExprMST x, fromExprMST y)

instance (ShaderInput a, ShaderInput b, ShaderType a, ShaderType b) =>
        ShaderInput (a, b) where
        buildMST f i = ((f i, f (i + 1)), i + 2)

instance MultiShaderType a => MultiShaderType [a] where
        type ExprMST [x] = [ExprMST x]
        mapMST f = map $ mapMST f
        foldrMST f = foldr . flip $ foldrMST f
        toExprMST = map toExprMST
        fromExprMST = map fromExprMST

{-
-- Data.HList.HList
-- forse è meglio lasciar perdere e preferire sempre i generics?
instance All MultiShaderType as => MultiShaderType (HList as) where
        foldrMST f s HNil = s
        foldrMST f s (HCons x xs) = foldrMST f (foldrMST f s xs) x
-}

hashMST :: MultiShaderType a => a -> a
hashMST = mapMST (fromExpr . HashDummy . hash . toExpr)

hashListMST :: MultiShaderType a => a -> [Int]
hashListMST = foldrMST (\x l -> hash (toExpr x) : l) []

type UniformID = Int
-- TODO: use an existential type instead?
type UniformValue = (String, UniformLocation -> GL ())
type Unistate = (UniformID, [(UniformID, UniformValue)])

data Shader (s :: ShaderStage) i o =
        Shader ((Unistate, i) -> (Unistate, o))         -- Real shader function
               ((UniformID, i) -> (UniformID, o))       -- Shader function optimized for hashing

instance Category (Shader s) where
        Shader f hf . Shader g hg = Shader (f . g) (hf . hg)
        id = Shader id id

instance Arrow (Shader s) where
        arr f = Shader (second f) (second f)
        Shader f hf *** Shader g hg = Shader (split f g) (split hf hg)
                where split f g (s, (fin, gin)) = let (s', fout) = f (s, fin)
                                                      (s'', gout) = g (s', gin)
                                                  in (s'', (fout, gout))

instance ArrowApply (Shader s) where
        app = Shader (\(s, (Shader f _, i)) -> f (s, i))
                     (\(s, (Shader _ hf, i)) -> hf (s, i))

instance (ShaderInput i, MultiShaderType o) => Hashable (Shader s i o) where
        hashWithSalt salt (Shader _ hf) =
                let (input, _) = buildMST (fromExpr . Input) 0
                    (_, output) = hf (0, input)
                in hashWithSalt salt $ hashListMST output

data ShaderStage = VertexShaderStage | FragmentShaderStage
type VertexShader = Shader VertexShaderStage
type FragmentShader = Shader FragmentShaderStage

newtype UniformSetter x = UniformSetter { unUniformSetter :: x }

uniformList :: Shader s i o -> [(UniformID, UniformValue)]
uniformList (Shader f _) = let err = "uniformList: input must not be evaluated"
                               ((_, map), _) = f ((0, []), error err)
                           in map

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
         => (x -> CPUBase u)
         -> Shader s (u, i) o
         -> Shader s (UniformSetter x, i) o
uniform' f shader = first (f . unUniformSetter) ^>> uniform shader

-- | Add a uniform and directly set it with the second operand.
infixl 9 ~~
(~~) :: BaseUniform u => Shader s (u, i) o -> CPUBase u -> Shader s i o
shader ~~ u = const u &&& id ^>> uniform shader

-- | Like ('~~') but uses a 'UniformSetter'.
withUniformSetter :: BaseUniform u
                  => UniformSetter x
                  -> (x -> CPUBase u)
                  -> Shader s (u, i) o
                  -> Shader s i o
withUniformSetter u f shader = const u &&& id ^>> uniform' f shader

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
