{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds, KindSignatures,
             TypeFamilies, FlexibleContexts, UndecidableInstances,
             FlexibleInstances, DefaultSignatures, TypeOperators,
             MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Shader.Types where

import Control.Arrow
import Control.Category
import Data.Hashable
import Data.MemoTrie
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Internal.GL (GL, UniformLocation)
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language
import Graphics.Rendering.Ombra.Shader.Language.Types
import Prelude hiding (id, (.))

-- | A type that contains zero or more 'ShaderType's.
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

-- | A type that contains a finite amount of 'ShaderType's.
class MultiShaderType a => ShaderInput a where
        buildMST :: (forall x. ShaderType x => Int -> x) -> Int -> (a, Int)
        default buildMST :: (Generic a, GShaderInput (Rep a))
                         => (forall x. ShaderType x => Int -> x)
                         -> Int
                         -> (a, Int)
        buildMST f = first to . gbuildMST f

class ShaderInput a => Uniform a where
        type CPUUniform a
        foldrUniform :: Proxy a
                     -> (forall g. BaseUniform g
                                => Proxy g
                                -> CPUBase g
                                -> b
                                -> b)
                     -> b
                     -> CPUUniform a
                     -> b
        default foldrUniform :: ( Generic a
                                , Generic (CPUUniform a)
                                , GUniform (Rep a) (Rep (CPUUniform a))
                                )
                             => Proxy a
                             -> (forall g. BaseUniform g
                                        => Proxy g
                                        -> CPUBase g
                                        -> b
                                        -> b
                                )
                             -> b
                             -> CPUUniform a
                             -> b
        foldrUniform (Proxy :: Proxy a) f s u =
                gfoldrUniform (Proxy :: Proxy (Rep a)) f s $ from u


type UniformID = Int
-- TODO: use an existential type instead?
type UniformValue = (String, UniformLocation -> GL ())
type Unistate = (UniformID, [(UniformID, UniformValue)])

data Shader (s :: ShaderStage) i o =
        Shader ((Unistate, i) -> (Unistate, o))
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
type VertexShader = Shader VertexShaderStage
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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

instance MultiShaderType GSampler2D where
        type ExprMST GSampler2D = Expr
        mapMST f = f
        foldrMST f = flip f
        toExprMST = toExpr
        fromExprMST = fromExpr

instance ShaderInput GSampler2D where
        buildMST f i = (f i, i + 1)

instance GLES => Uniform GSampler2D where
        type CPUUniform GSampler2D = CPUBase GSampler2D
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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
        foldrUniform proxy f s u = f proxy u s

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

instance MultiShaderType a => MultiShaderType [a] where
        type ExprMST [x] = [ExprMST x]
        mapMST f = map $ mapMST f
        foldrMST f = foldr . flip $ foldrMST f
        toExprMST = map toExprMST
        fromExprMST = map fromExprMST

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
                      -> (forall g. BaseUniform g
                                 => Proxy g
                                 -> CPUBase g
                                 -> b
                                 -> b)
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

{-
... Data.HList.HList
instance All MultiShaderType as => MultiShaderType (HList as) where
        foldrMST f s HNil = s
        foldrMST f s (HCons x xs) = foldrMST f (foldrMST f s xs) x
-}

hashListMST :: MultiShaderType a => a -> [Int]
hashListMST = foldrMST (\x l -> hash (toExpr x) : l) []

uniformList :: Shader s i o -> [(UniformID, UniformValue)]
uniformList (Shader f _) = let err = "uniformList: input must not be evaluated"
                               ((_, map), _) = f ((0, []), error err)
                           in map
