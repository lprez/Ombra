{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             FlexibleInstances, UndecidableInstances, TypeFamilies, RankNTypes,
             GeneralizedNewtypeDeriving, ConstraintKinds,
             TypeFamilyDependencies #-}

module Graphics.Rendering.Ombra.Geometry.Types (
        GVertex(..),
        VertexAttributes(..),
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
import Data.Word (Word16)

import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.CPU

class Attributes (AttributeTypes a) => GVertex a where
        type AttributeTypes a :: [*]
        type Vertex a = v | v -> a
        toVertexAttributes :: Vertex a -> VertexAttributes (AttributeTypes a)
        fromVertexAttributes :: VertexAttributes (AttributeTypes a) -> Vertex a

{-
instance Attributes is => GVertex (HList is) where
        type AttributeTypes (VertexAttributes is) = is
        type Vertex (VertexAttributes is) = VertexAttributes is
        toVertexAttributes = id
        fromVertexAttributes = id
-}

data Triangle a = Triangle a a a

data VertexAttributes (is :: [*]) where
        Attr :: (Eq (CPUBase i), H.Hashable (CPUBase i), BaseAttribute i)
             => CPUBase i
             -> VertexAttributes '[i]
        (:~) :: VertexAttributes '[i]
             -> VertexAttributes is
             -> VertexAttributes (i ': is)

infixr 5 :~

data Elements is = Triangles Int [Triangle (AttrVertex is)]

-- | A set of triangles.
data Geometry g where
        Geometry :: GVertex g
                 => { topHash :: Int            -- TODO: ?
                    , geometryHash :: Int       -- TODO: ?
                    , top :: AttrCol (AttributeTypes g)
                    , elements :: Elements (AttributeTypes g)
                    , lastIndex :: Int
                    }
                 -> Geometry g

newtype GeometryBuilderT g m a = GeometryBuilderT (StateT (Geometry g) m a)
        deriving (Functor, Applicative, Monad, MonadTrans)

type GeometryBuilder g = GeometryBuilderT g Identity

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
        AttrTop :: (NotTop p, BaseAttribute i)
                => Int
                -> AttrTable Top is
                -> AttrTable p (i ': is)
                -> AttrTable Top (i ': is)
        AttrCell :: (H.Hashable (CPUBase i), BaseAttribute i)
                 => CPUBase i
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
        foldTop :: (forall i is. b -> AttrCol (i ': is) -> b)
                -> b
                -> AttrCol is
                -> b
        rowToVertexAttributes :: NotTop p
                              => AttrTable p is
                              -> VertexAttributes is

instance (BaseAttribute i, Eq (CPUBase i)) => Attributes '[i] where
        emptyAttrCol = AttrTop (H.hash (0 :: Int)) AttrNil AttrEnd
        cell (Attr x) down = AttrCell x AttrNil down
        addTop v@(Attr x) (AttrTop thash next down) =
                AttrTop (H.hashWithSalt (H.hash x + thash) thash)
                        next
                        (cell v down)
        foldTop f acc top = f acc top
        rowToVertexAttributes (AttrCell x _ _) = Attr x

instance (BaseAttribute i1, Eq (CPUBase i1), Attributes (i2 ': is)) =>
        Attributes (i1 ': (i2 ': is)) where
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

instance Functor Triangle where
        fmap f (Triangle x y z) = Triangle (f x) (f y) (f z)

instance H.Hashable (VertexAttributes is) where
        hashWithSalt s (Attr a) = H.hashWithSalt s a
        hashWithSalt s (x :~ y) = H.hashWithSalt (H.hashWithSalt s x) y

instance Eq (VertexAttributes is) where
        (Attr x) == (Attr x') = x == x'
        (Attr x :~ v) == (Attr x' :~ v') = x == x' && v == v'

instance H.Hashable a => H.Hashable (Triangle a) where
        hashWithSalt salt (Triangle x y z) = H.hashWithSalt salt (x, y, z)

instance Eq (Geometry is) where
        g == g' = geometryHash g == geometryHash g'

instance H.Hashable (Geometry is) where
        hashWithSalt salt = H.hashWithSalt salt . geometryHash

instance H.Hashable (Elements is) where
        hashWithSalt salt (Triangles h _) = H.hashWithSalt salt h

instance Eq (Elements is) where
        (Triangles h _) == (Triangles h' _) = h == h'

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
