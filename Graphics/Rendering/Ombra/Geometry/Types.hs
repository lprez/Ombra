{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             FlexibleInstances, UndecidableInstances, TypeFamilies, RankNTypes,
             GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Graphics.Rendering.Ombra.Geometry.Types (
        Vertex(..),
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

import Graphics.Rendering.Ombra.Shader.CPU

data Triangle a = Triangle a a a

-- | A list of the attributes of a vertex.
--
-- For instance: @Attr Position3 p :~ Attr UV u :~ Attr Normal3 n@
data Vertex (is :: [*]) where
        Attr :: (Eq (CPU S i), H.Hashable (CPU S i), Attribute S i)
             => (a -> i)
             -> CPU S i
             -> Vertex '[i]
        (:~) :: Vertex '[i] -> Vertex is -> Vertex (i ': is)

infixr 5 :~

data Elements is = Triangles Int [Triangle (AttrVertex is)]

-- | A set of triangles.
data Geometry (is :: [*]) where
        Geometry :: Attributes is
                 => { topHash :: Int            -- TODO: ?
                    , geometryHash :: Int       -- TODO: ?
                    , top :: AttrCol is
                    , elements :: Elements is
                    , lastIndex :: Int
                    }
                 -> Geometry is

newtype GeometryBuilderT is m a = GeometryBuilderT (StateT (Geometry is) m a)
        deriving (Functor, Applicative, Monad, MonadTrans)

type GeometryBuilder is = GeometryBuilderT is Identity

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
        AttrTop :: (NotTop p, Attribute 'S i)
                => Int
                -> AttrTable Top is
                -> AttrTable p (i ': is)
                -> AttrTable Top (i ': is)
        AttrCell :: (H.Hashable (CPU S i), Attribute 'S i)
                 => CPU 'S i
                 -> AttrTable (Previous p) is
                 -> AttrTable p (i ': is)
                 -> AttrTable (Previous p) (i ': is)

type AttrCol = AttrTable Top
type NotTop p = Previous p ~ Previous p

class Attributes is where
        emptyAttrCol :: AttrCol is
        cell :: Vertex is -> AttrTable p is -> AttrTable (Previous p) is
        addTop :: Vertex is -> AttrCol is -> AttrCol is
        foldTop :: (forall i is. b -> AttrCol (i ': is) -> b)
                -> b
                -> AttrCol is
                -> b
        rowToVertex :: NotTop p => AttrTable p is -> Vertex is

instance (Attribute 'S i, Eq (CPU 'S i)) => Attributes '[i] where
        emptyAttrCol = AttrTop (H.hash (0 :: Int)) AttrNil AttrEnd
        cell (Attr _ x) down = AttrCell x AttrNil down
        addTop v@(Attr _ x) (AttrTop thash next down) =
                AttrTop (H.hashWithSalt (H.hash x + thash) thash)
                        next
                        (cell v down)
        foldTop f acc top = f acc top
        rowToVertex (AttrCell x _ _) = Attr (const undefined) x

instance (Attribute 'S i1, Eq (CPU 'S i1), Attributes (i2 ': is)) =>
        Attributes (i1 ': (i2 ': is)) where
        emptyAttrCol = AttrTop (H.hash (0 :: Int)) emptyAttrCol AttrEnd
        cell (Attr _ x :~ v) down1@(AttrCell _ down2 _) =
                AttrCell x (cell v down2) down1
        cell (Attr _ x :~ v) AttrEnd = AttrCell x (cell v AttrEnd) AttrEnd
        addTop v1@(Attr _ x :~ v2) (AttrTop thash next down) =
                AttrTop (H.hashWithSalt (H.hash x + thash) thash)
                        (addTop v2 next)
                        (cell v1 down)
        foldTop f acc top@(AttrTop _ next _) = foldTop f (f acc top) next
        rowToVertex (AttrCell x next _) =
                Attr (const undefined) x :~ rowToVertex next

instance Functor Triangle where
        fmap f (Triangle x y z) = Triangle (f x) (f y) (f z)

instance H.Hashable (Vertex is) where
        hashWithSalt s (Attr _ a) = H.hashWithSalt s a
        hashWithSalt s (x :~ y) = H.hashWithSalt (H.hashWithSalt s x) y

instance Eq (Vertex is) where
        (Attr _ x) == (Attr _ x') = x == x'
        (Attr _ x :~ v) == (Attr _ x' :~ v') = x == x' && v == v'

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
