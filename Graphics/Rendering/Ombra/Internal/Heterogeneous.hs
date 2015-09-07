{-# LANGUAGE GADTs, PolyKinds, DataKinds, KindSignatures, TypeOperators,
             MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             ConstraintKinds, TypeFamilies, UndecidableInstances,
             ScopedTypeVariables, OverlappingInstances, RankNTypes #-} -- ???

-- HSet?
module Graphics.Rendering.Ombra.Internal.Heterogeneous (
        HSet(..),
        HList(..),
        Has,
        Hasnt,
        HasMany,
        All,
        Append,
        Union,
        Remove,
        Map,
        HSetGet,
        HSetGets,
        (|!),
        hsGet,
        hsGets,
        hlMap
) where

-- extremely inefficient
data HSet :: [*] -> * where
        ES :: HSet '[]
        (:|) :: Hasnt x ts => x -> HSet ts -> HSet (x ': ts)

data HList :: [*] -> * where
        EL :: HList '[]
        (:-) :: x -> HList ts -> HList (x ': ts)

-- useless?
data TNat = TZ | TS TNat

data Nat :: TNat -> * where
        Z :: Nat TZ
        S :: Nat n -> Nat (TS n)

infixr 4 :|
infixr 4 :-

(|!) :: HSet ts -> Nat n -> At n ts
(x :| _) |! Z = x
(_ :| xs) |! (S n) = xs |! n

class HSetGet y (xs :: [*]) where
        hsGet :: HSet xs -> y

instance HSetGet x (x ': xs) where
        hsGet (x :| _) = x

instance HSetGet y xs => HSetGet y (x ': xs) where
        hsGet (_ :| xs) = hsGet xs

class HSetGets ys xs where
        hsGets :: HSet xs -> HSet ys

instance HSetGets '[] xs where
        hsGets _ = ES

instance (HSetGet y xs, HSetGets ys xs, Hasnt y ys) =>
         HSetGets (y ': ys) xs where
        hsGets xs = (hsGet xs :: y) :| (hsGets xs :: HSet ys)

{-
hlMap :: t (tf :: * -> *) -> (forall x. x -> tf x)
      -> HList xs -> HList (Map tf xs)
hlMap _ _ EL = EL
hlMap tf f (x :- xs) = f x :- hlMap tf f xs
-}

-- useless
class BoolBridge (b :: Bool) where bool :: t b -> Bool
instance BoolBridge 'True where bool _ = True
instance BoolBridge 'False where bool _ = False

-- useless ?
class NatBridge (n :: TNat) where
        natConv :: t n -> Nat n

instance NatBridge TZ where
        natConv = const Z

instance NatBridge n => NatBridge (TS n) where
        natConv = const . S $ natConv (undefined :: Nat n)

type family At (n :: TNat) (xs :: [*]) where
        At TZ (x ': xs) = x
        At (TS n) (x ': xs) = At n xs

type family Index x (xs :: [*]) :: TNat where
        Index x (x ': xs) = TZ
        Index x (y ': xs) = TS (Index x xs)

type family If (b :: Bool) (x :: k) (y :: k) :: k where
        If True x y = x
        If False x y = y

type family Elem x (xs :: [*]) :: Bool where
        Elem x (x ': xs) = True
        Elem x (y ': xs) = Elem x xs
        Elem x xs = False

type family Nub (xs :: [*]) :: [*] where
        Nub xs = Nub' xs '[]

type family Nub' (xs :: [*]) (ys :: [*]) where
        Nub' '[] ys = '[]
        Nub' (x ': xs) ys = If (Elem x ys)
                               (Nub' xs ys)
                               (x ': (Nub' xs (x ': ys)))

type family Append (xs :: [*]) (ys :: [*]) where
        Append '[] ys = ys
        Append (x ': xs) ys = x ': (Append xs ys)

-- crap
type family Union (xs :: [*]) (ys :: [*]) where
        Union xs ys = Nub (Append xs ys)

type family Remove y (xs :: [*]) where
        Remove x (x ': xs) = Remove x xs
        Remove y (x ': xs) = y ': Remove y xs

type family Map (f :: k -> k') (xs :: [k]) where
        Map f '[] = '[]
        Map f (x ': xs) = f x ': Map f xs

type Has x xs = Elem x xs ~ 'True
type Hasnt x xs = Elem x xs ~ 'False

class Ever (t :: k)
instance Ever x

-- useless
class All constr (xs :: [k])
instance All constr '[]
instance (All constr xs, constr x) => All constr (x ': xs)

type family HasMany (xs :: [*]) (ys :: [*]) where
        HasMany '[] ys = Ever ys
        HasMany (x ': xs) ys = (Has x ys, HasMany xs ys)

instance Show (HSet '[]) where
        show ES = "ES"

instance (Show t, Show (HSet ts)) => Show (HSet (t ': ts)) where
        show (x :| xs) = show x ++ " :| " ++ show xs

instance Show (HList '[]) where
        show EL = "EL"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
        show (x :- xs) = show x ++ " :- " ++ show xs
