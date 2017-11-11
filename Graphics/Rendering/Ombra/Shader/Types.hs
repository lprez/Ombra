{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds, KindSignatures,
             TypeFamilies, FlexibleContexts, UndecidableInstances,
             FlexibleInstances, DefaultSignatures, TypeOperators,
             MultiParamTypeClasses, FunctionalDependencies,
             ExistentialQuantification #-}

module Graphics.Rendering.Ombra.Shader.Types where

import Control.Arrow
import Control.Category
import Data.Hashable
import Data.MemoTrie
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Texture (Texture)
import Prelude hiding (id, (.))

-- | Types that contain zero or more 'ShaderType's.
class HasTrie (ExprMST a) => MultiShaderType a where
        type ExprMST a
        type ExprMST a = GExprMST (Rep a)

        mapMST :: (forall x. ShaderType x => x -> x) -> a -> a
        default mapMST :: (Generic a, GMultiShaderType (Rep a))
                       => (forall x. ShaderType x => x -> x)
                       -> a
                       -> a
        mapMST f = to . gmapMST f . from

        toExprMST :: a -> ExprMST a
        default toExprMST :: ( Generic a
                             , GMultiShaderType (Rep a)
                             , ExprMST a ~ GExprMST (Rep a)
                             )
                          => a
                          -> ExprMST a
        toExprMST = gtoExprMST . from

        fromExprMST :: ExprMST a -> a
        default fromExprMST :: ( Generic a
                               , GMultiShaderType (Rep a)
                               , ExprMST a ~ GExprMST (Rep a)
                               )
                            => ExprMST a
                            -> a
        fromExprMST = to . gfromExprMST

-- | Types that contain a finite amount of 'ShaderType's.
class MultiShaderType a => ShaderInput a where
        buildMST :: (forall x. ShaderType x => Int -> x) -> Int -> (a, Int)
        default buildMST :: (Generic a, GShaderInput (Rep a))
                         => (forall x. ShaderType x => Int -> x)
                         -> Int
                         -> (a, Int)
        buildMST f = first to . gbuildMST f

        foldrMST :: (forall x. ShaderType x => x -> b -> b) -> b -> a -> b
        default foldrMST :: (Generic a, GShaderInput (Rep a))
                         => (forall x. ShaderType x => x -> b -> b)
                         -> b
                         -> a
                         -> b
        foldrMST f s = gfoldrMST f s . from

-- | Types that contain uniform values.
class ShaderInput a => Uniform a where
        type CPUUniform a
        foldrUniform :: Proxy a
                     -> (UniformValue -> b -> b)
                     -> b
                     -> CPUUniform a
                     -> b
        default foldrUniform :: ( Generic a
                                , Generic (CPUUniform a)
                                , GUniform (Rep a) (Rep (CPUUniform a))
                                )
                             => Proxy a
                             -> (UniformValue -> b -> b)
                             -> b
                             -> CPUUniform a
                             -> b
        foldrUniform (Proxy :: Proxy a) f s u =
                gfoldrUniform (Proxy :: Proxy (Rep a)) f s $ from u

buildMST' :: ShaderInput a
          => (forall x. ShaderType x => String -> Int -> x)
          -> Int
          -> (a, Int)
buildMST' f = buildMST (f (typeName (undefined :: x))
                                :: forall x. ShaderType x => Int -> x)

fromGVec4s :: FragmentShaderOutput o => [GVec4] -> o
fromGVec4s = fst . fromGFloats . toGFloatsList
        where toGFloatsList [] = []
              toGFloatsList (GVec4 x y z w : xs) =
                      x : y : z : w : toGFloatsList xs

textureCount :: (FragmentShaderOutput o, Integral b) => Proxy o -> b
textureCount (_ :: Proxy o) = let nFloats = natVal (Proxy :: Proxy (NFloats o))
                              in ceiling (fromIntegral nFloats / 4)

toGVec4s :: FragmentShaderOutput o => o -> [GVec4]
toGVec4s = toGVec4sList . flip toGFloats []
        where toGVec4sList [] = []
              toGVec4sList [x] = [GVec4 x 0 0 0]
              toGVec4sList [x, y] = [GVec4 x y 0 0]
              toGVec4sList [x, y, z] = [GVec4 x y z 0]
              toGVec4sList (x : y : z : w : xs) =
                      GVec4 x y z w : toGVec4sList xs

-- | Types that contain 'GFloat's.
class (ShaderInput o, KnownNat (NFloats o)) => FragmentShaderOutput o where
        type NFloats o :: Nat
        type NFloats o = GNFloats (Rep o)

        fromGFloats :: [GFloat] -> (o, [GFloat])
        default fromGFloats :: (Generic o, GFragmentShaderOutput (Rep o))
                            => [GFloat]
                            -> (o, [GFloat])
        fromGFloats = first to . gfromGFloats

        toGFloats :: o -> [GFloat] -> [GFloat]
        default toGFloats :: (Generic o, GFragmentShaderOutput (Rep o))
                          => o
                          -> [GFloat]
                          -> [GFloat]
        toGFloats x = gtoGFloats $ from x


class MapShader f s | f -> s where
        mapShader :: Shader s i o -> f i -> f o

type UniformID = Int
data UniformValue = forall g. BaseUniform g => UniformValue (Proxy g)
                                                            (CPUBase g)
                  | UniformTexture Texture

data ShaderState = ShaderState UniformID
                               [(UniformID, UniformValue)]
                               [Texture]

-- | A function that runs in the GPU.
data Shader (s :: ShaderStage) i o =
        Shader ((ShaderState, i) -> (ShaderState, o))
               ((UniformID, i) -> (UniformID, o))

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

instance ArrowChoice (Shader s) where
        left = leftApp

instance (ShaderInput i, ShaderInput o) => Hashable (Shader s i o) where
        hashWithSalt salt (Shader _ hf) =
                let (input, _) = buildMST' (\t -> fromExpr . Input t) 0
                    (_, output) = hf (0, input)
                    outHash = foldrMST (\x l -> hash (toExpr x) : l) [] output
                in hashWithSalt salt outHash

data ShaderStage = VertexShaderStage | FragmentShaderStage

-- | A shader that transforms vertices.
type VertexShader = Shader VertexShaderStage

-- | A shader that transforms fragments.
type FragmentShader = Shader FragmentShaderStage

data Fragment = Fragment {
        -- | The coordinates of the fragment.
        fragCoord :: GVec4,
        -- | If the fragment belongs to a front-facing primitive.
        fragFrontFacing :: GBool,
        -- | Partial derivative of the argument with respect to the window X
        -- coordinate.
        dFdx :: forall a. GenType a => a -> a,
        -- | Partial derivative of the argument with respect to the window Y
        -- coordinate.
        dFdy :: forall a. GenType a => a -> a,
        -- | Sum of the absolute values of 'dFdx' and 'dFdy'.
        fwidth :: forall a. GenType a => a -> a
}

instance MultiShaderType GBool where
        type ExprMST GBool = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBool where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GBool where
        type CPUUniform GBool = CPUBase GBool
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GBool where
        newtype (GBool :->: b) = GBoolTrie (ExprMST GBool :->: b)
        trie f = GBoolTrie $ trie (f . fromExprMST)
        untrie (GBoolTrie t) = untrie t . toExprMST
        enumerate (GBoolTrie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GFloat where
        type ExprMST GFloat = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GFloat where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GFloat where
        type CPUUniform GFloat = CPUBase GFloat
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GFloat where
        type NFloats GFloat = 1
        fromGFloats (x : xs) = (x, xs)
        toGFloats x = (x :)

instance HasTrie GFloat where
        newtype (GFloat :->: b) = GFloatTrie (ExprMST GFloat :->: b)
        trie f = GFloatTrie $ trie (f . fromExprMST)
        untrie (GFloatTrie t) = untrie t . toExprMST
        enumerate (GFloatTrie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GInt where
        type ExprMST GInt = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GInt where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GInt where
        type CPUUniform GInt = CPUBase GInt
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GInt where
        newtype (GInt :->: b) = GIntTrie (ExprMST GInt :->: b)
        trie f = GIntTrie $ trie (f . fromExprMST)
        untrie (GIntTrie t) = untrie t . toExprMST
        enumerate (GIntTrie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GSampler2D where
        type ExprMST GSampler2D = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GSampler2D where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GSampler2D where
        type CPUUniform GSampler2D = Texture
        foldrUniform _ f s u = f (UniformTexture u) s

instance HasTrie GSampler2D where
        newtype (GSampler2D :->: b) = GSampler2DTrie (ExprMST GSampler2D :->: b)
        trie f = GSampler2DTrie $ trie (f . fromExprMST)
        untrie (GSampler2DTrie t) = untrie t . toExprMST
        enumerate (GSampler2DTrie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GVec2 where
        type ExprMST GVec2 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec2 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GVec2 where
        type CPUUniform GVec2 = CPUBase GVec2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GVec2 where
        type NFloats GVec2 = 2
        fromGFloats (x : y : xs) = (GVec2 x y, xs)
        toGFloats (GVec2 x y) xs = x : y : xs

instance HasTrie GVec2 where
        newtype (GVec2 :->: b) = GVec2Trie (ExprMST GVec2 :->: b)
        trie f = GVec2Trie $ trie (f . fromExprMST)
        untrie (GVec2Trie t) = untrie t . toExprMST
        enumerate (GVec2Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GVec3 where
        type ExprMST GVec3 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec3 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GVec3 where
        type CPUUniform GVec3 = CPUBase GVec3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GVec3 where
        type NFloats GVec3 = 3
        fromGFloats (x : y : z : xs) = (GVec3 x y z, xs)
        toGFloats (GVec3 x y z) xs = x : y : z : xs

instance HasTrie GVec3 where
        newtype (GVec3 :->: b) = GVec3Trie (ExprMST GVec3 :->: b)
        trie f = GVec3Trie $ trie (f . fromExprMST)
        untrie (GVec3Trie t) = untrie t . toExprMST
        enumerate (GVec3Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GVec4 where
        type ExprMST GVec4 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec4 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GVec4 where
        type CPUUniform GVec4 = CPUBase GVec4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GVec4 where
        type NFloats GVec4 = 4
        fromGFloats (x : y : z : w : xs) = (GVec4 x y z w, xs)
        toGFloats (GVec4 x y z w) xs = x : y : z : w : xs

instance HasTrie GVec4 where
        newtype (GVec4 :->: b) = GVec4Trie (ExprMST GVec4 :->: b)
        trie f = GVec4Trie $ trie (f . fromExprMST)
        untrie (GVec4Trie t) = untrie t . toExprMST
        enumerate (GVec4Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GIVec2 where
        type ExprMST GIVec2 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec2 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GIVec2 where
        type CPUUniform GIVec2 = CPUBase GIVec2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GIVec2 where
        newtype (GIVec2 :->: b) = GIVec2Trie (ExprMST GIVec2 :->: b)
        trie f = GIVec2Trie $ trie (f . fromExprMST)
        untrie (GIVec2Trie t) = untrie t . toExprMST
        enumerate (GIVec2Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GIVec3 where
        type ExprMST GIVec3 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec3 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GIVec3 where
        type CPUUniform GIVec3 = CPUBase GIVec3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GIVec3 where
        newtype (GIVec3 :->: b) = GIVec3Trie (ExprMST GIVec3 :->: b)
        trie f = GIVec3Trie $ trie (f . fromExprMST)
        untrie (GIVec3Trie t) = untrie t . toExprMST
        enumerate (GIVec3Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GIVec4 where
        type ExprMST GIVec4 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec4 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GIVec4 where
        type CPUUniform GIVec4 = CPUBase GIVec4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GIVec4 where
        newtype (GIVec4 :->: b) = GIVec4Trie (ExprMST GIVec4 :->: b)
        trie f = GIVec4Trie $ trie (f . fromExprMST)
        untrie (GIVec4Trie t) = untrie t . toExprMST
        enumerate (GIVec4Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GBVec2 where
        type ExprMST GBVec2 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec2 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GBVec2 where
        type CPUUniform GBVec2 = CPUBase GBVec2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GBVec2 where
        newtype (GBVec2 :->: b) = GBVec2Trie (ExprMST GBVec2 :->: b)
        trie f = GBVec2Trie $ trie (f . fromExprMST)
        untrie (GBVec2Trie t) = untrie t . toExprMST
        enumerate (GBVec2Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GBVec3 where
        type ExprMST GBVec3 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec3 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GBVec3 where
        type CPUUniform GBVec3 = CPUBase GBVec3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GBVec3 where
        newtype (GBVec3 :->: b) = GBVec3Trie (ExprMST GBVec3 :->: b)
        trie f = GBVec3Trie $ trie (f . fromExprMST)
        untrie (GBVec3Trie t) = untrie t . toExprMST
        enumerate (GBVec3Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GBVec4 where
        type ExprMST GBVec4 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec4 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GBVec4 where
        type CPUUniform GBVec4 = CPUBase GBVec4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GBVec4 where
        newtype (GBVec4 :->: b) = GBVec4Trie (ExprMST GBVec4 :->: b)
        trie f = GBVec4Trie $ trie (f . fromExprMST)
        untrie (GBVec4Trie t) = untrie t . toExprMST
        enumerate (GBVec4Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GMat2 where
        type ExprMST GMat2 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat2 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GMat2 where
        type CPUUniform GMat2 = CPUBase GMat2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GMat2 where
        newtype (GMat2 :->: b) = GMat2Trie (ExprMST GMat2 :->: b)
        trie f = GMat2Trie $ trie (f . fromExprMST)
        untrie (GMat2Trie t) = untrie t . toExprMST
        enumerate (GMat2Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GMat3 where
        type ExprMST GMat3 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat3 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GMat3 where
        type CPUUniform GMat3 = CPUBase GMat3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GMat3 where
        newtype (GMat3 :->: b) = GMat3Trie (ExprMST GMat3 :->: b)
        trie f = GMat3Trie $ trie (f . fromExprMST)
        untrie (GMat3Trie t) = untrie t . toExprMST
        enumerate (GMat3Trie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType GMat4 where
        type ExprMST GMat4 = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat4 where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance GLES => Uniform GMat4 where
        type CPUUniform GMat4 = CPUBase GMat4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance HasTrie GMat4 where
        newtype (GMat4 :->: b) = GMat4Trie (ExprMST GMat4 :->: b)
        trie f = GMat4Trie $ trie (f . fromExprMST)
        untrie (GMat4Trie t) = untrie t . toExprMST
        enumerate (GMat4Trie t) = map (first fromExprMST) (enumerate t)

instance (KnownNat n, ShaderType t) => MultiShaderType (GArray n t) where
        type ExprMST (GArray n t) = Expr
        mapMST f = f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance (KnownNat n, ShaderType t) => ShaderInput (GArray n t) where
        buildMST f i = (f i, i + 1)
        foldrMST f = flip f

instance (KnownNat n, ShaderType t, BaseUniform (GArray n t), GLES) =>
        Uniform (GArray n t) where
        type CPUUniform (GArray n t) = CPUBase (GArray n t)
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance (KnownNat n, ShaderType t) => HasTrie (GArray n t) where
        newtype (GArray n t :->: b) = GArrayTrie (ExprMST (GArray n t) :->: b)
        trie f = GArrayTrie $ trie (f . fromExprMST)
        untrie (GArrayTrie t) = untrie t . toExprMST
        enumerate (GArrayTrie t) = map (first fromExprMST) (enumerate t)

instance MultiShaderType () where
        type ExprMST () = ()
        mapMST _ = id
        toExprMST = id
        fromExprMST = id

instance ShaderInput () where
        buildMST _ i = ((), i)
        foldrMST _ x _ = x

instance Uniform () where
        type CPUUniform () = ()
        foldrUniform _ _ s _ = s

instance FragmentShaderOutput () where
        type NFloats () = 0
        fromGFloats xs = ((), xs)
        toGFloats () = id

instance (MultiShaderType a, MultiShaderType b) => MultiShaderType (a, b) where
        type ExprMST (a, b) = (ExprMST a, ExprMST b)
        mapMST f (x, y) = (mapMST f x, mapMST f y)
        toExprMST (x, y) = (toExprMST x, toExprMST y)
        fromExprMST (x, y) = (fromExprMST x, fromExprMST y)

instance (MultiShaderType a, MultiShaderType b, MultiShaderType c) =>
        MultiShaderType (a, b, c) where
        type ExprMST (a, b, c) = (ExprMST a, ExprMST b, ExprMST c)
        mapMST f (x, y, z) = (mapMST f x, mapMST f y, mapMST f z)
        toExprMST (x, y, z) = (toExprMST x, toExprMST y, toExprMST z)
        fromExprMST (x, y, z) = (fromExprMST x, fromExprMST y, fromExprMST z)

instance (ShaderInput a, ShaderInput b) => ShaderInput (a, b) where
        buildMST f i = let (a, i') = buildMST f i
                           (b, i'') = buildMST f i'
                       in ((a, b), i'')
        foldrMST f s (x, y) = foldrMST f (foldrMST f s y) x

instance ( ShaderInput a, ShaderInput b, ShaderInput c
         ) => ShaderInput (a, b, c) where
        buildMST f i = let (a, i1) = buildMST f i
                           (b, i2) = buildMST f i1
                           (c, i3) = buildMST f i2
                       in ((a, b, c), i3)
        foldrMST f s (x, y, z) = foldrMST f (foldrMST f (foldrMST f s z) y) x

instance (Uniform a, Uniform b) => Uniform (a, b) where
        type CPUUniform (a, b) = (CPUUniform a, CPUUniform b)
        foldrUniform (Proxy :: Proxy (a, b)) f s (a, b) =
                foldrUniform (Proxy :: Proxy a)
                             f
                             (foldrUniform (Proxy :: Proxy b) f s b)
                             a
                             

instance (Uniform a, Uniform b, Uniform c) => Uniform (a, b, c) where
        type CPUUniform (a, b, c) = (CPUUniform a, CPUUniform b, CPUUniform c)
        foldrUniform (Proxy :: Proxy (a, b, c)) f s (a, b, c) =
                let s' = foldrUniform (Proxy :: Proxy c) f s c
                    s'' = foldrUniform (Proxy :: Proxy b) f s' b
                in foldrUniform (Proxy :: Proxy a) f s'' a

instance ( FragmentShaderOutput a
         , FragmentShaderOutput b
         , KnownNat (NFloats a + NFloats b)
         ) =>
        FragmentShaderOutput (a, b) where
        type NFloats (a, b) = NFloats a + NFloats b
        fromGFloats xs = let (x, xs') = fromGFloats xs
                             (y, xs'') = fromGFloats xs'
                        in ((x, y), xs'')
        toGFloats (x, y) = toGFloats x . toGFloats y

instance ( FragmentShaderOutput a
         , FragmentShaderOutput b
         , FragmentShaderOutput c
         , KnownNat (NFloats a + NFloats b + NFloats c)
         ) => FragmentShaderOutput (a, b, c) where
        type NFloats (a, b, c) = NFloats a + NFloats b + NFloats c
        fromGFloats xs = let (x, xs1) = fromGFloats xs
                             (y, xs2) = fromGFloats xs1
                             (z, xs3) = fromGFloats xs2
                        in ((x, y, z), xs3)
        toGFloats (x, y, z) = toGFloats x . toGFloats y . toGFloats z

instance MultiShaderType a => MultiShaderType [a] where
        type ExprMST [a] = [ExprMST a]
        mapMST f = map $ mapMST f
        toExprMST = map toExprMST
        fromExprMST = map fromExprMST

instance (ShaderInput a, MultiShaderType b) => MultiShaderType (a -> b) where
        type ExprMST (a -> b) = ExprMST b
        mapMST f g = \x -> mapMST f $ g x
        toExprMST = toExprMST . dummyFun
        fromExprMST x = const $ fromExprMST x

instance (ShaderInput a, MultiShaderType b) => HasTrie (a -> b) where
        newtype ((a -> b) :->: c) = FunTrie (ExprMST (a -> b) :->: c)
        trie f = FunTrie $ trie (f . fromExprMST)
        untrie (FunTrie t) = untrie t . toExprMST
        enumerate (FunTrie t) = map (first fromExprMST) (enumerate t)

instance (ShaderInput a, MultiShaderType b) =>
        MultiShaderType (Shader s a b) where
        type ExprMST (Shader s a b) = (ExprMST b, UniformID)
        mapMST f s = s >>^ mapMST f
        toExprMST (Shader _ hf) = let err = "This shader can't be used as MST"
                                      (uid, _) = hf (0, error err)
                                      out = snd . dummyFun $ hf . (,) 0
                                  in (toExprMST out, uid)
        fromExprMST (out, dif) = let hf (uid, _) = (uid + dif, fromExprMST out)
                                     f (ShaderState uid u t, _) =
                                          ( ShaderState (uid + dif) u t
                                          , fromExprMST out
                                          )
                                 in Shader f hf

instance (ShaderInput a, MultiShaderType b) => HasTrie (Shader s a b) where
        newtype (Shader s a b :->: c) = SFunTrie (ExprMST (Shader s a b) :->: c)
        trie f = SFunTrie $ trie (f . fromExprMST)
        untrie (SFunTrie t) = untrie t . toExprMST
        enumerate (SFunTrie t) = map (first fromExprMST) (enumerate t)

dummyFun :: ShaderInput a => (a -> b) -> b
dummyFun g = g . fst $ buildMST (fromExpr . Dummy) 0

class GMultiShaderType (g :: * -> *) where
        type GExprMST g :: *
        gmapMST :: (forall x. ShaderType x => x -> x) -> g p -> g p
        gtoExprMST :: g p -> GExprMST g
        gfromExprMST :: GExprMST g -> g p

instance GMultiShaderType a => GMultiShaderType (M1 i d a) where
        type GExprMST (M1 i d a) = GExprMST a
        gmapMST f (M1 x) = M1 $ gmapMST f x
        gtoExprMST (M1 x) = gtoExprMST x
        gfromExprMST x = M1 $ gfromExprMST x

instance MultiShaderType c => GMultiShaderType (K1 i c) where
        type GExprMST (K1 i c) = ExprMST c
        gmapMST f (K1 x) = K1 $ mapMST f x
        gtoExprMST (K1 x) = toExprMST x
        gfromExprMST x = K1 $ fromExprMST x

instance (GMultiShaderType a, GMultiShaderType b) =>
        GMultiShaderType (a :*: b) where
        type GExprMST (a :*: b) = (GExprMST a, GExprMST b)
        gmapMST f (a :*: b) = gmapMST f a :*: gmapMST f b
        gtoExprMST (a :*: b) = (gtoExprMST a, gtoExprMST b)
        gfromExprMST (a, b) = gfromExprMST a :*: gfromExprMST b

class GShaderInput g where
        gbuildMST :: (forall x. ShaderType x => Int -> x) -> Int -> (g p, Int)
        gfoldrMST :: (forall x. ShaderType x => x -> b -> b) -> b -> g p -> b

instance GShaderInput a => GShaderInput (M1 i d a) where
        gbuildMST f = first M1 . gbuildMST f
        gfoldrMST f s (M1 x) = gfoldrMST f s x

instance ShaderInput c => GShaderInput (K1 i c) where
        gbuildMST f = first K1 . buildMST f
        gfoldrMST f s (K1 x) = foldrMST f s x

instance (GShaderInput a, GShaderInput b) => GShaderInput (a :*: b) where
        gbuildMST f i = let (a, i') = gbuildMST f i
                            (b, i'') = gbuildMST f i'
                        in (a :*: b, i'')
        gfoldrMST f s (a :*: b) = gfoldrMST f (gfoldrMST f s b) a

class GUniform (a :: * -> *) (c :: * -> *) where
        gfoldrUniform :: Proxy a
                      -> (UniformValue -> b -> b)
                      -> b
                      -> c p
                      -> b

instance GUniform a c => GUniform (M1 i d a) (M1 i' d' c) where
        gfoldrUniform (Proxy :: Proxy (M1 i d a)) f s (M1 u) =
                gfoldrUniform (Proxy :: Proxy a) f s u

instance (Uniform a, c ~ CPUUniform a) => GUniform (K1 i a) (K1 i' c) where
        gfoldrUniform (Proxy :: Proxy (K1 i a)) f s (K1 u) =
                foldrUniform (Proxy :: Proxy a) f s u

instance (GUniform a c, GUniform a' c') => GUniform (a :*: a') (c :*: c') where
        gfoldrUniform (Proxy :: Proxy (a :*: a')) f s (u :*: u') =
                gfoldrUniform (Proxy :: Proxy a)
                              f
                              (gfoldrUniform (Proxy :: Proxy a') f s u')
                              u

class GFragmentShaderOutput g where
        type GNFloats g :: Nat
        gfromGFloats :: [GFloat] -> (g p, [GFloat])
        gtoGFloats :: g p -> [GFloat] -> [GFloat]

instance GFragmentShaderOutput a => GFragmentShaderOutput (M1 i d a) where
        type GNFloats (M1 i d a) = GNFloats a
        gfromGFloats = first M1 . gfromGFloats
        gtoGFloats (M1 x) = gtoGFloats x

instance FragmentShaderOutput a => GFragmentShaderOutput (K1 i a) where
        type GNFloats (K1 i a) = NFloats a
        gfromGFloats = first K1 . fromGFloats
        gtoGFloats (K1 x) = toGFloats x

instance (GFragmentShaderOutput a, GFragmentShaderOutput b) =>
        GFragmentShaderOutput (a :*: b) where
        type GNFloats (a :*: b) = GNFloats a + GNFloats b
        gfromGFloats xs = let (x, xs') = gfromGFloats xs
                              (y, xs'') = gfromGFloats xs'
                          in (x :*: y, xs'')
        gtoGFloats (x :*: y) = gtoGFloats x . gtoGFloats y

uniformList :: ShaderInput i
            => Shader s i o
            -> UniformID
            -> (UniformID, [(UniformID, UniformValue)], [Texture])
uniformList (Shader f _) uid =
        let (input, _) = buildMST (fromExpr . Dummy) 0
            ((ShaderState uid' umap tmap), _) = f ( ShaderState uid [] []
                                                  , input
                                                  )
        in (uid', umap, tmap)
