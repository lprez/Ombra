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


        foldrMST :: (forall x. ShaderType x => x -> b -> b) -> b -> a -> b
        default foldrMST :: (Generic a, GMultiShaderType (Rep a))
                         => (forall x. ShaderType x => x -> b -> b)
                         -> b
                         -> a
                         -> b
        foldrMST f s = gfoldrMST f s . from

        toExprMST :: a -> ExprMST a
        default toExprMST :: (Generic a, GMultiShaderType (Rep a))
                          => a
                          -> GExprMST (Rep a)
        toExprMST = gtoExprMST . from

        fromExprMST :: ExprMST a -> a
        default fromExprMST :: (Generic a, GMultiShaderType (Rep a))
                            => GExprMST (Rep a)
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

fromGVec4s :: FragmentShaderOutput o => [GVec4] -> o
fromGVec4s = fst . fromGFloats . toGFloatsList
        where toGFloatsList [] = []
              toGFloatsList (GVec4 x y z w : xs) =
                      x : y : z : w : toGFloatsList xs

textureCount :: FragmentShaderOutput o => Proxy o -> Integer
textureCount (_ :: Proxy o) = natVal (Proxy :: Proxy (NFloats o)) `quot` 4

toGVec4s :: FragmentShaderOutput o => o -> [GVec4]
toGVec4s = reverse . toGVec4sList . flip toGFloats []
        where toGVec4sList [] = []
              toGVec4sList [x] = [GVec4 x 0 0 0]
              toGVec4sList [x, y] = [GVec4 x y 0 0]
              toGVec4sList [x, y, z] = [GVec4 x y z 0]
              toGVec4sList (x : y : z : w : xs) =
                      GVec4 x y z w : toGVec4sList xs

-- | Types that contain 'GFloat's.
class (MultiShaderType o, KnownNat (NFloats o)) => FragmentShaderOutput o where
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

instance (ShaderInput i, MultiShaderType o) => Hashable (Shader s i o) where
        hashWithSalt salt (Shader _ hf) =
                let (input, _) = buildMST (fromExpr . Input) 0
                    (_, output) = hf (0, input)
                in hashWithSalt salt $ hashListMST output

data ShaderStage = VertexShaderStage | FragmentShaderStage

-- | A shader that transforms vertices.
type VertexShader = Shader VertexShaderStage

-- | A shader that transforms fragments.
type FragmentShader = Shader FragmentShaderStage

instance MultiShaderType GBool where
        type ExprMST GBool = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBool where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GBool where
        type CPUUniform GBool = CPUBase GBool
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GFloat where
        type ExprMST GFloat = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GFloat where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GFloat where
        type CPUUniform GFloat = CPUBase GFloat
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GFloat where
        type NFloats GFloat = 1
        fromGFloats (x : xs) = (x, xs)
        toGFloats x = (x :)

instance MultiShaderType GInt where
        type ExprMST GInt = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GInt where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GInt where
        type CPUUniform GInt = CPUBase GInt
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GSampler2D where
        type ExprMST GSampler2D = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GSampler2D where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GSampler2D where
        type CPUUniform GSampler2D = Texture
        foldrUniform _ f s u = f (UniformTexture u) s

instance MultiShaderType GVec2 where
        type ExprMST GVec2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec2 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GVec2 where
        type CPUUniform GVec2 = CPUBase GVec2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GVec2 where
        type NFloats GVec2 = 2
        fromGFloats (x : y : xs) = (GVec2 x y, xs)
        toGFloats (GVec2 x y) xs = x : y : xs

instance MultiShaderType GVec3 where
        type ExprMST GVec3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec3 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GVec3 where
        type CPUUniform GVec3 = CPUBase GVec3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GVec3 where
        type NFloats GVec3 = 3
        fromGFloats (x : y : z : xs) = (GVec3 x y z, xs)
        toGFloats (GVec3 x y z) xs = x : y : z : xs

instance MultiShaderType GVec4 where
        type ExprMST GVec4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GVec4 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GVec4 where
        type CPUUniform GVec4 = CPUBase GVec4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance FragmentShaderOutput GVec4 where
        type NFloats GVec4 = 4
        fromGFloats (x : y : z : w : xs) = (GVec4 x y z w, xs)
        toGFloats (GVec4 x y z w) xs = x : y : z : w : xs

instance MultiShaderType GIVec2 where
        type ExprMST GIVec2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec2 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GIVec2 where
        type CPUUniform GIVec2 = CPUBase GIVec2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GIVec3 where
        type ExprMST GIVec3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec3 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GIVec3 where
        type CPUUniform GIVec3 = CPUBase GIVec3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GIVec4 where
        type ExprMST GIVec4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GIVec4 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GIVec4 where
        type CPUUniform GIVec4 = CPUBase GIVec4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GBVec2 where
        type ExprMST GBVec2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec2 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GBVec2 where
        type CPUUniform GBVec2 = CPUBase GBVec2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GBVec3 where
        type ExprMST GBVec3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec3 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GBVec3 where
        type CPUUniform GBVec3 = CPUBase GBVec3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GBVec4 where
        type ExprMST GBVec4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GBVec4 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GBVec4 where
        type CPUUniform GBVec4 = CPUBase GBVec4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GMat2 where
        type ExprMST GMat2 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat2 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GMat2 where
        type CPUUniform GMat2 = CPUBase GMat2
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GMat3 where
        type ExprMST GMat3 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat3 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GMat3 where
        type CPUUniform GMat3 = CPUBase GMat3
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType GMat4 where
        type ExprMST GMat4 = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GMat4 where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GMat4 where
        type CPUUniform GMat4 = CPUBase GMat4
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance (KnownNat n, ShaderType t) => MultiShaderType (GArray n t) where
        type ExprMST (GArray n t) = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance (KnownNat n, ShaderType t) => ShaderInput (GArray n t) where
        buildMST f i = (f i, i + 1)

instance (KnownNat n, ShaderType t, BaseUniform (GArray n t), GLES) =>
        Uniform (GArray n t) where
        type CPUUniform (GArray n t) = CPUBase (GArray n t)
        foldrUniform proxy f s u = f (UniformValue proxy u) s

instance MultiShaderType () where
        type ExprMST () = ()
        mapMST _ = id
        foldrMST _ x _ = x
        toExprMST = id
        fromExprMST = id

instance ShaderInput () where
        buildMST _ i = ((), i)

instance Uniform () where
        type CPUUniform () = ()
        foldrUniform _ _ s _ = s

instance FragmentShaderOutput () where
        type NFloats () = 0
        fromGFloats xs = ((), xs)
        toGFloats () = id

instance (MultiShaderType a, MultiShaderType b) => MultiShaderType (a, b) where
        type ExprMST (x, y) = (ExprMST x, ExprMST y)
        mapMST f (x, y) = (mapMST f x, mapMST f y)
        foldrMST f s (x, y) = foldrMST f (foldrMST f s y) x
        toExprMST (x, y) = (toExprMST x, toExprMST y)
        fromExprMST (x, y) = (fromExprMST x, fromExprMST y)

instance (MultiShaderType a, MultiShaderType b, MultiShaderType c) =>
        MultiShaderType (a, b, c) where
        type ExprMST (x, y, z) = (ExprMST x, ExprMST y, ExprMST z)
        mapMST f (x, y, z) = (mapMST f x, mapMST f y, mapMST f z)
        foldrMST f s (x, y, z) = foldrMST f (foldrMST f (foldrMST f s z) y) x
        toExprMST (x, y, z) = (toExprMST x, toExprMST y, toExprMST z)
        fromExprMST (x, y, z) = (fromExprMST x, fromExprMST y, fromExprMST z)

instance (ShaderInput a, ShaderInput b) => ShaderInput (a, b) where
        buildMST f i = let (a, i') = buildMST f i
                           (b, i'') = buildMST f i'
                       in ((a, b), i'')

instance ( ShaderInput a, ShaderInput b, ShaderInput c
         ) => ShaderInput (a, b, c) where
        buildMST f i = let (a, i1) = buildMST f i
                           (b, i2) = buildMST f i1
                           (c, i3) = buildMST f i2
                       in ((a, b, c), i3)

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
        type ExprMST [x] = [ExprMST x]
        mapMST f = map $ mapMST f
        foldrMST f = foldr . flip $ foldrMST f
        toExprMST = map toExprMST
        fromExprMST = map fromExprMST

instance (ShaderInput a, MultiShaderType b) => MultiShaderType (a -> b) where
        type ExprMST (a -> b) = ExprMST b
        mapMST f g = \x -> mapMST f $ g x
        foldrMST f s = foldrMST f s . dummyFun
        toExprMST = toExprMST . dummyFun
        fromExprMST x = const $ fromExprMST x

dummyFun :: ShaderInput a => (a -> b) -> b
dummyFun g = g . fst $ buildMST (fromExpr . Dummy) 0

class GMultiShaderType (g :: * -> *) where
        type GExprMST g :: *
        gmapMST :: (forall x. ShaderType x => x -> x) -> g p -> g p
        gfoldrMST :: (forall x. ShaderType x => x -> b -> b) -> b -> g p -> b
        gtoExprMST :: g p -> GExprMST g
        gfromExprMST :: GExprMST g -> g p

instance GMultiShaderType a => GMultiShaderType (M1 i d a) where
        type GExprMST (M1 i d a) = GExprMST a
        gmapMST f (M1 x) = M1 $ gmapMST f x
        gfoldrMST f s (M1 x) = gfoldrMST f s x
        gtoExprMST (M1 x) = gtoExprMST x
        gfromExprMST x = M1 $ gfromExprMST x

instance MultiShaderType c => GMultiShaderType (K1 i c) where
        type GExprMST (K1 i c) = ExprMST c
        gmapMST f (K1 x) = K1 $ mapMST f x
        gfoldrMST f s (K1 x) = foldrMST f s x
        gtoExprMST (K1 x) = toExprMST x
        gfromExprMST x = K1 $ fromExprMST x

instance (GMultiShaderType a, GMultiShaderType b) =>
        GMultiShaderType (a :*: b) where
        type GExprMST (a :*: b) = (GExprMST a, GExprMST b)
        gmapMST f (a :*: b) = gmapMST f a :*: gmapMST f b
        gfoldrMST f s (a :*: b) = gfoldrMST f (gfoldrMST f s b) a
        gtoExprMST (a :*: b) = (gtoExprMST a, gtoExprMST b)
        gfromExprMST (a, b) = gfromExprMST a :*: gfromExprMST b

class GShaderInput g where
        gbuildMST :: (forall x. ShaderType x => Int -> x) -> Int -> (g p, Int)

instance GShaderInput a => GShaderInput (M1 i d a) where
        gbuildMST f = first M1 . gbuildMST f

instance ShaderInput c => GShaderInput (K1 i c) where
        gbuildMST f = first K1 . buildMST f

instance (GShaderInput a, GShaderInput b) => GShaderInput (a :*: b) where
        gbuildMST f i = let (a, i') = gbuildMST f i
                            (b, i'') = gbuildMST f i'
                        in (a :*: b, i'')

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

hashListMST :: MultiShaderType a => a -> [Int]
hashListMST = foldrMST (\x l -> hash (toExpr x) : l) []

uniformList :: Shader s i o -> ([(UniformID, UniformValue)], [Texture])
uniformList (Shader f _) = let err = "uniformList: input must not be evaluated"
                               zeroState = ShaderState 0 [] []
                               ((ShaderState _ umap tmap), _) = f ( zeroState
                                                                  , error err
                                                                  )
                           in (umap, tmap)
