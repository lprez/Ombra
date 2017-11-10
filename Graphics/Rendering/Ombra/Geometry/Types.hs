{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             FlexibleInstances, TypeFamilies, RankNTypes, UndecidableInstances,
             GeneralizedNewtypeDeriving, ConstraintKinds, MultiParamTypeClasses,
             TypeFamilyDependencies, ScopedTypeVariables, DefaultSignatures,
             CPP #-}

module Graphics.Rendering.Ombra.Geometry.Types (
        GeometryVertex(..),
        GGeometryVertex(..),
        VertexAttributes(..),
        ElementType(..),
        Point(..),
        Line(..),
        Triangle(..),
        AttrTable(..),
        AttrCol,
        AttrVertex(..),
        AttrPosition(..),
        Geometry(..),
        GeometryBuilder,
        GeometryBuilderT(..),
        Elements(..),
        NotTop,
        Previous,
        Attributes(..)
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import GHC.Exts (Constraint)
import qualified Data.Hashable as H
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.Word (Word16)
import Data.Proxy
import GHC.Generics

import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Vector

-- | Types that can be used as 'Geometry' vertices.
class Attributes (AttributeTypes a) => GeometryVertex a where
        type AttributeTypes a :: [*]
        type AttributeTypes a = GAttributeTypes (Rep a) (Rep (Vertex a))

        type Vertex a = v | v -> a

        toVertexAttributes :: Vertex a -> VertexAttributes (AttributeTypes a)
        default toVertexAttributes :: ( Generic a
                                      , Generic (Vertex a)
                                      , GGeometryVertex (Rep a) (Rep (Vertex a))
                                      , VertexAttributes (AttributeTypes a) ~ 
                                        VertexAttributes
                                            (GAttributeTypes (Rep a)
                                                             (Rep (Vertex a)))

                                      )
                                   => Vertex a
                                   -> VertexAttributes (AttributeTypes a)
        toVertexAttributes = gtoVertexAttributes (Proxy :: Proxy (Rep a)) . from

        fromVertexAttributes :: VertexAttributes (AttributeTypes a) -> Vertex a
        default fromVertexAttributes :: ( Generic a
                                        , Generic (Vertex a)
                                        , GGeometryVertex (Rep a)
                                                          (Rep (Vertex a))
                                        , VertexAttributes (AttributeTypes a) ~ 
                                          VertexAttributes
                                              (GAttributeTypes (Rep a)
                                                               (Rep (Vertex a)))

                                        )
                                   => VertexAttributes (AttributeTypes a)
                                   -> Vertex a
        fromVertexAttributes =
                to . gfromVertexAttributes (Proxy :: Proxy (Rep a))

class (Functor e, Foldable e) => ElementType e where
        elementType :: GLES => proxy e -> GLEnum
        elementFromList :: [a] -> e a

instance ElementType Point where
        elementType _ = gl_POINTS
        elementFromList [x] = Point x

instance ElementType Line where
        elementType _ = gl_LINES
        elementFromList [x, y] = Line x y

instance ElementType Triangle where
        elementType _ = gl_TRIANGLES
        elementFromList [x, y, z] = Triangle x y z

data Point a = Point a
data Line a = Line a a
data Triangle a = Triangle a a a

-- TODO: use AttrTable rows instead
data VertexAttributes (is :: [*]) where
        Attr :: (Eq (CPUBase i), H.Hashable (CPUBase i), BaseAttribute i)
             => CPUBase i
             -> VertexAttributes '[i]
        (:~) :: VertexAttributes '[i]
             -> VertexAttributes is
             -> VertexAttributes (i ': is)

infixr 5 :~

data Elements e is = Elements Int [e (AttrVertex is)]

-- | A set of triangles.
data Geometry e g where
        Geometry :: { topHash :: Int            -- TODO: ?
                    , geometryHash :: Int       -- TODO: ?
                    , top :: AttrCol (AttributeTypes g)
                    , elements :: Elements e (AttributeTypes g)
                    , lastIndex :: Int
                    }
                 -> Geometry e g

newtype GeometryBuilderT e g m a = GeometryBuilderT (StateT (Geometry e g) m a)
        deriving (Functor, Applicative, Monad, MonadTrans)

type GeometryBuilder e g = GeometryBuilderT e g Identity

-- | A vertex in a 'Geometry'.
data AttrVertex (is :: [*]) where
        AttrVertex :: NotTop p => Word16 -> AttrTable p is -> AttrVertex is

data AttrPosition = Top | Middle | Bottom | End

type family Previous (p :: AttrPosition) :: AttrPosition where
        Previous Middle = Middle
        Previous Bottom = Middle
        Previous End = Bottom

-- | A table where rows are vertices and columns are the values of a certain
-- attribute. The top row contains the hash of the values in the row instead of
-- actual attribute data.
data AttrTable (b :: AttrPosition) (is :: [*]) where
        AttrNil :: AttrTable b '[]
        AttrEnd :: AttrTable End is
        AttrTop :: NotTop p
                => Int
                -> AttrTable Top is
                -> AttrTable p (i ': is)
                -> AttrTable Top (i ': is)
        AttrCell :: CPUBase i
                 -> AttrTable (Previous p) is
                 -> AttrTable p (i ': is)
                 -> AttrTable (Previous p) (i ': is)

type AttrCol = AttrTable Top
type NotTop p = Previous p ~ Previous p

class Empty is ~ 'False => Attributes is where
        emptyAttrCol :: AttrCol is
        cell :: VertexAttributes is
             -> AttrTable p is
             -> AttrTable (Previous p) is
        addTop :: VertexAttributes is -> AttrCol is -> AttrCol is
        foldTop :: (forall i is. BaseAttribute i => b -> AttrCol (i ': is) -> b)
                -> b
                -> AttrCol is
                -> b
        rowToVertexAttributes :: NotTop p
                              => AttrTable p is
                              -> VertexAttributes is

instance (BaseAttribute i, Eq (CPUBase i), H.Hashable (CPUBase i)) =>
        Attributes '[i] where
        emptyAttrCol = AttrTop (H.hash (0 :: Int)) AttrNil AttrEnd
        cell (Attr x) down = AttrCell x AttrNil down
        addTop v@(Attr x) (AttrTop thash next down) =
                AttrTop (H.hashWithSalt (H.hash x + thash) thash)
                        next
                        (cell v down)
        foldTop f acc top = f acc top
        rowToVertexAttributes (AttrCell x _ _) = Attr x

instance ( BaseAttribute i1
         , Eq (CPUBase i1)
         , Attributes (i2 ': is)
         , H.Hashable (CPUBase i1)
         ) => Attributes (i1 ': (i2 ': is)) where
        emptyAttrCol = AttrTop (H.hash (0 :: Int)) emptyAttrCol AttrEnd
        cell (Attr x :~ v) down1@(AttrCell _ down2 _) =
                AttrCell x (cell v down2) down1
        cell (Attr x :~ v) AttrEnd = AttrCell x (cell v AttrEnd) AttrEnd
        addTop v1@(Attr x :~ v2) (AttrTop thash next down) =
                AttrTop (H.hashWithSalt (H.hash x + thash) thash)
                        (addTop v2 next)
                        (cell v1 down)
        foldTop f acc top@(AttrTop _ next _) = foldTop f (f acc top) next
        rowToVertexAttributes (AttrCell x next _) =
                Attr x :~ rowToVertexAttributes next

instance Functor Point where
        fmap f (Point x) = Point (f x)

instance Functor Line where
        fmap f (Line x y) = Line (f x) (f y)

instance Functor Triangle where
        fmap f (Triangle x y z) = Triangle (f x) (f y) (f z)

instance H.Hashable (VertexAttributes is) where
        hashWithSalt s (Attr a) = H.hashWithSalt s a
        hashWithSalt s (x :~ y) = H.hashWithSalt (H.hashWithSalt s x) y

instance Eq (VertexAttributes is) where
        (Attr x) == (Attr x') = x == x'
        (Attr x :~ v) == (Attr x' :~ v') = x == x' && v == v'

instance H.Hashable a => H.Hashable (Point a) where
        hashWithSalt salt (Point x) = H.hashWithSalt salt x

instance H.Hashable a => H.Hashable (Line a) where
        hashWithSalt salt (Line x y) = H.hashWithSalt salt (x, y)

instance H.Hashable a => H.Hashable (Triangle a) where
        hashWithSalt salt (Triangle x y z) = H.hashWithSalt salt (x, y, z)

instance Foldable Point where
        foldr f s (Point x) = f x s

instance Foldable Line where
        foldr f s (Line x y) = f x $ f y s

instance Foldable Triangle where
        foldr f s (Triangle x y z) = f x $ f y $ f z s

instance Eq (Geometry e is) where
        g == g' = geometryHash g == geometryHash g'

instance H.Hashable (Geometry e is) where
        hashWithSalt salt = H.hashWithSalt salt . geometryHash

instance H.Hashable (Elements e is) where
        hashWithSalt salt (Elements h _) = H.hashWithSalt salt h

instance Eq (Elements e is) where
        (Elements h _) == (Elements h' _) = h == h'

instance H.Hashable (AttrVertex is) where
        hashWithSalt salt (AttrVertex i _) = H.hashWithSalt salt i

instance Eq (AttrVertex is) where
        (AttrVertex i _) == (AttrVertex i' _) = i == i'

instance H.Hashable (AttrCol is) where
        hashWithSalt salt AttrNil = salt
        hashWithSalt salt (AttrTop thash next _) =
                H.hashWithSalt (H.hashWithSalt salt thash) next

instance Eq (AttrCol (i ': is)) where
        (AttrTop h _ _) == (AttrTop h' _ _) = h == h'

class (Attributes as, Attributes bs, Attributes (Append as bs)) =>
        BreakVertex (as :: [*]) (bs :: [*]) where
        breakVertexAttributes :: VertexAttributes (Append as bs)
                              -> (VertexAttributes as, VertexAttributes bs)

instance (Attributes '[a], Attributes bs, Attributes (a ': bs)) =>
        BreakVertex '[a] bs where
        breakVertexAttributes (attr :~ rest) = (attr, rest)

instance ( Attributes (a1 ': a2 ': as)
         , BreakVertex (a2 ': as) bs
         , Attributes (Append (a1 ': a2 ': as) bs)
         ) => BreakVertex (a1 ': a2 ': as) bs where
        breakVertexAttributes (a1 :~ rest) =
                let (a2as, bs) = breakVertexAttributes rest
                in (a1 :~ a2as, bs)

class (Attributes as, Attributes bs, Attributes (Append as bs)) =>
        AppendVertex (as :: [*]) (bs :: [*]) where
        appendVertexAttributes :: VertexAttributes as
                               -> VertexAttributes bs
                               -> VertexAttributes (Append as bs)

instance (Attributes '[a], Attributes bs, Attributes (a ': bs)) =>
        AppendVertex '[a] bs where
        appendVertexAttributes x@(Attr _) xs = x :~ xs

instance ( Attributes (a1 ': a2 ': as)
         , Attributes (a1 ': Append (a2 ': as) bs)
         , AppendVertex (a2 ': as) bs
         ) => AppendVertex (a1 ': a2 ': as) bs where
        appendVertexAttributes (x@(Attr _) :~ xs1) xs2 =
                x :~ appendVertexAttributes xs1 xs2

class GGeometryVertex (g :: * -> *) (v :: * -> *) where
        type GAttributeTypes g v :: [*]
        gtoVertexAttributes :: Proxy g
                            -> v p
                            -> VertexAttributes (GAttributeTypes g v)
        gfromVertexAttributes :: Proxy g
                              -> VertexAttributes (GAttributeTypes g v)
                              -> v p

instance GGeometryVertex c v => GGeometryVertex (M1 i d c) (M1 i' d' v) where
        type GAttributeTypes (M1 i d c) (M1 i' d' v) = GAttributeTypes c v
        gtoVertexAttributes (Proxy :: Proxy (M1 i d c)) (M1 v) =
                gtoVertexAttributes (Proxy :: Proxy c) v
        gfromVertexAttributes (Proxy :: Proxy (M1 i d c)) va =
                M1 $ gfromVertexAttributes (Proxy :: Proxy c) va

instance (GeometryVertex c, v ~ Vertex c) =>
        GGeometryVertex (K1 i c) (K1 i v) where
        type GAttributeTypes (K1 i c) (K1 i v) = AttributeTypes c
        gtoVertexAttributes _ (K1 v) = toVertexAttributes v
        gfromVertexAttributes _ va = K1 $ fromVertexAttributes va

instance ( GGeometryVertex c v
         , GGeometryVertex c' v'
         , AppendVertex (GAttributeTypes c v) (GAttributeTypes c' v')
         , BreakVertex (GAttributeTypes c v) (GAttributeTypes c' v')
         ) => GGeometryVertex (c :*: c') (v :*: v') where
        type GAttributeTypes (c :*: c') (v :*: v') =
                Append (GAttributeTypes c v) (GAttributeTypes c' v')
        gtoVertexAttributes (Proxy :: Proxy (c :*: c')) (v :*: v') =
                let va = gtoVertexAttributes (Proxy :: Proxy c) v
                    va' = gtoVertexAttributes (Proxy :: Proxy c') v'
                in appendVertexAttributes va va'
        gfromVertexAttributes (Proxy :: Proxy (c :*: c')) va =
                let (vaa, vab) = breakVertexAttributes va
                in gfromVertexAttributes (Proxy :: Proxy c) vaa :*:
                   gfromVertexAttributes (Proxy :: Proxy c') vab

instance ( GeometryVertex a
         , GeometryVertex b
         , BreakVertex (AttributeTypes a) (AttributeTypes b)
         , AppendVertex (AttributeTypes a) (AttributeTypes b)
         ) => GeometryVertex (a, b) where
        type AttributeTypes (a, b) = Append (AttributeTypes a)
                                            (AttributeTypes b)
        type Vertex (a, b) = (Vertex a, Vertex b)
        toVertexAttributes (a, b) =
                appendVertexAttributes (toVertexAttributes a)
                                       (toVertexAttributes b)
        fromVertexAttributes v = let (va, vb) = breakVertexAttributes v
                                 in ( fromVertexAttributes va
                                    , fromVertexAttributes vb
                                    )

instance ( GeometryVertex a
         , GeometryVertex b
         , GeometryVertex c
         , BreakVertex (AttributeTypes a) (Append (AttributeTypes b)
                                                  (AttributeTypes c))
         , BreakVertex (AttributeTypes b) (AttributeTypes c)
         , AppendVertex (AttributeTypes a) (Append (AttributeTypes b)
                                                   (AttributeTypes c))
         , AppendVertex (AttributeTypes b) (AttributeTypes c)
         ) => GeometryVertex (a, b, c) where
        type AttributeTypes (a, b, c) = Append (AttributeTypes a)
                                               (Append (AttributeTypes b)
                                                       (AttributeTypes c))
        type Vertex (a, b, c) = (Vertex a, Vertex b, Vertex c)
        toVertexAttributes (a, b, c) =
                appendVertexAttributes (toVertexAttributes a) $
                        appendVertexAttributes (toVertexAttributes b)
                                               (toVertexAttributes c)
        fromVertexAttributes v = let (va, vbc) = breakVertexAttributes v
                                     (vb, vc) = breakVertexAttributes vbc
                                 in ( fromVertexAttributes va
                                    , fromVertexAttributes vb
                                    , fromVertexAttributes vc
                                    )

instance GLES => GeometryVertex GFloat where
        type AttributeTypes GFloat = '[GFloat]
        type Vertex GFloat = Float
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GBool where
        type AttributeTypes GBool = '[GBool]
        type Vertex GBool = Bool
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GInt where
        type AttributeTypes GInt = '[GInt]
        type Vertex GInt = Int32
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GVec2 where
        type AttributeTypes GVec2 = '[GVec2]
        type Vertex GVec2 = Vec2
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GVec3 where
        type AttributeTypes GVec3 = '[GVec3]
        type Vertex GVec3 = Vec3
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GVec4 where
        type AttributeTypes GVec4 = '[GVec4]
        type Vertex GVec4 = Vec4
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GIVec2 where
        type AttributeTypes GIVec2 = '[GIVec2]
        type Vertex GIVec2 = IVec2
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GIVec3 where
        type AttributeTypes GIVec3 = '[GIVec3]
        type Vertex GIVec3 = IVec3
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x

instance GLES => GeometryVertex GIVec4 where
        type AttributeTypes GIVec4 = '[GIVec4]
        type Vertex GIVec4 = IVec4
        toVertexAttributes x = Attr x
        fromVertexAttributes (Attr x) = x
