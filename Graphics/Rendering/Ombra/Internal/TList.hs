{-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses, TypeFamilies,
             TypeOperators, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ConstraintKinds, PolyKinds #-}

module Graphics.Rendering.Ombra.Internal.TList (
        Empty,
        Equal,
        Member,
        IsMember,
        Subset,
        IsSubset,
        Remove,
        Difference,
        Append,
        Insert,
        Reverse,
        Union,
        Set
) where

type Set xs = Union xs xs ~ xs

type family Empty (xs :: [*]) :: Bool where
        Empty '[] = True
        Empty (x ': xs) = False

type Equal xs ys = And (IsSubset xs ys) (IsSubset ys xs) ~ True

type Member x xs = IsMember x xs ~ True

type NotMember x xs = IsMember x xs ~ False

type family IsMember x (xs :: [*]) :: Bool where
        IsMember x '[] = False
        IsMember x (x ': xs) = True
        IsMember y (x ': xs) = IsMember y xs

type family IsSubset (xs :: [*]) (ys :: [*]) :: Bool where
        IsSubset xs xs = True
        IsSubset '[] ys = True
        IsSubset (x ': xs) ys = And (IsMember x ys) (IsSubset xs ys)

class Subset (xs :: [*]) (ys :: [*])
instance IsSubset xs ys ~ 'True => Subset xs ys
type family Remove x (xs :: [*]) where
        Remove x '[] = '[]
        Remove x (x ': xs) = Remove x xs
        Remove x (y ': xs) = y ': Remove x xs

type family Difference (xs :: [*]) (ys :: [*]) where
        Difference xs '[] = xs
        Difference xs (y ': ys) = Difference (Remove y xs) ys

type family Append (xs :: [*]) (ys :: [*]) where
        Append '[] ys = ys
        Append (x ': xs) ys = x ': Append xs ys

type family Insert y (xs :: [*]) where
        Insert y '[] = '[y]
        Insert y (y ': xs) = y ': xs
        Insert y (x ': xs) = x ': Insert y xs

type Reverse xs = Reverse' xs '[]

type family Reverse' (xs :: [*]) (ys :: [*]) where
        Reverse' '[] ys = ys
        Reverse' (x ': xs) ys = Reverse' xs (x ': ys)

type family Union (xs :: [*]) (ys :: [*]) where
        Union '[] ys = ys
        Union (x ': xs) ys = Union xs (Insert x ys)

type family And (a :: Bool) (b :: Bool) :: Bool where
        And True True = True
        And a b = False
