{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators,
             RankNTypes, FlexibleContexts, ScopedTypeVariables,
             MultiParamTypeClasses, FlexibleInstances, ConstraintKinds #-}

module Graphics.Rendering.Ombra.Shader.Shader (
        Shader(..),
        Valid,
        Member,
        AllTypeable,
        Subset,
        Equal,
        Union,
        Insert,
        STList(..),
        stFold,
        staticList,
        staticSTList
) where

import Data.Typeable
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.Language.Types (ShaderType)
import Prelude (String, error, Bool(False), undefined)

infixr 4 :-

-- | An heterogeneous set of 'ShaderType's and 'Typeable's.
data STList :: [*] -> * where
        N :: STList '[]
        (:-) :: (ShaderType a, Typeable a, IsMember a xs ~ False)
             => a -> STList xs -> STList (a ': xs)

-- | The condition for a valid 'Shader'.
type Valid gs is os = ( StaticList gs, StaticList is, StaticList os
                      , StaticSTList gs, StaticSTList is, StaticSTList os)

-- | A function from a (heterogeneous) set of uniforms and a set of inputs
-- (attributes or varyings) to a set of outputs (varyings).
type Shader gs is os = STList gs -> STList is -> STList os

stFold :: (forall x. (Typeable x, ShaderType x) => acc -> x -> acc)
       -> acc -> STList xs -> acc
stFold _ acc N = acc
stFold f acc (x :- xs) = stFold f (f acc x) xs

class StaticList (xs :: [*]) where
        staticList :: Proxy (xs :: [*])
                   -> (forall x. (Typeable x, ShaderType x) => x -> y)
                   -> [y]

instance StaticList '[] where
        staticList (_ :: Proxy '[]) _ = []

instance (ShaderType x, Typeable x, StaticList xs) => StaticList (x ': xs) where
        staticList (_ :: Proxy (x ': xs)) f =
                f (undefined :: x) : staticList (undefined :: Proxy xs) f

class StaticSTList (xs :: [*]) where
        staticSTList :: Proxy (xs :: [*])
                     -> (forall x. (Typeable x, ShaderType x) => x -> x)
                     -> STList xs

instance StaticSTList '[] where
        staticSTList (_ :: Proxy '[]) _ = N

instance (ShaderType x, Typeable x, StaticSTList xs, IsMember x xs ~ False) =>
         StaticSTList (x ': xs) where
        staticSTList (_ :: Proxy (x ': xs)) f =
                f (undefined :: x) :- staticSTList (undefined :: Proxy xs) f

class AllTypeable (xs :: [*])
instance AllTypeable '[]
instance (Typeable x, AllTypeable xs) => AllTypeable (x ': xs)
