{-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses, TypeFamilies,
             TypeOperators, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ConstraintKinds, PolyKinds,
             ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Internal.TList (
        Not,
        Empty,
        Equal,
        IsEqual,
        EqualOrErr,
        Member,
        IsMember,
        NotMemberOrErr,
        Subset,
        IsSubset,
        Remove,
        Difference,
        Append,
        Insert,
        Reverse,
        Union,
        Set,
        module GHC.TypeLits
) where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Exts (Constraint)

type Set xs = Union xs xs ~ xs

type family Empty (xs :: [*]) :: Bool where
        Empty '[] = True
        Empty (x ': xs) = False

type family Not (a :: Bool) :: Bool where
        Not True = False
        Not False = True

type IsEqual xs ys = And (IsSubset xs ys) (IsSubset ys xs)
type Equal xs ys = IsEqual xs ys ~ True
type EqualOrErr xs ys err = TrueOrErr (IsEqual xs ys) err

type family TrueOrErr (a :: Bool) (err :: ErrorMessage) :: Constraint where
        TrueOrErr False err = TypeError err
        TrueOrErr a err = a ~ True

type FalseOrErr a err = TrueOrErr (Not a) err

type Member x xs = IsMember x xs ~ True
type MemberOrErr x xs err = TrueOrErr (IsMember x xs) err
type NotMemberOrErr x xs err = FalseOrErr (IsMember x xs) err

type family IsMember x (xs :: [*]) :: Bool where
        IsMember x '[] = False
        IsMember x (x ': xs) = True
        IsMember y (x ': xs) = IsMember y xs

type family IsSubset (xs :: [*]) (ys :: [*]) :: Bool where
        IsSubset xs xs = True
        IsSubset '[] ys = True
        IsSubset (x ': xs) ys = And (IsMember x ys) (IsSubset xs ys)

type Subset xs ys = TrueOrErr (IsSubset xs ys)
                              (Text "‘" :<>: ShowType xs :<>:
                               Text "’ is not a subset of ‘" :<>:
                               ShowType ys :<>: Text "’")

-- class Subset (xs :: [*]) (ys :: [*])
-- instance IsSubset xs ys ~ 'True => Subset xs ys

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
