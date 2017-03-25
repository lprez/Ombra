{-# LANGUAGE DeriveGeneric, TypeFamilies, TypeFamilyDependencies #-}

module Graphics.Rendering.Ombra.Vector (
        module Data.VectorSpace,
        module Data.Cross,
        Vec2(..),
        Vec3(..),
        Vec4(..),
        Mat2(..),
        Mat3(..),
        Mat4(..),
        IVec2(..),
        IVec3(..),
        IVec4(..),
        Matrix(..)
) where

import Data.Cross
import Data.VectorSpace
import Data.Hashable
import Data.Int
import Foreign.Storable
import Foreign.Ptr (castPtr)
import GHC.Generics

data Vec2 = Vec2 !Float !Float deriving Generic
data Vec3 = Vec3 !Float !Float !Float deriving Generic
data Vec4 = Vec4 !Float !Float !Float !Float deriving Generic

data Mat2 = Mat2 !Vec2 !Vec2 deriving Generic
data Mat3 = Mat3 !Vec3 !Vec3 !Vec3 deriving Generic
data Mat4 = Mat4 !Vec4 !Vec4 !Vec4 !Vec4 deriving Generic

data IVec2 = IVec2 !Int32 !Int32 deriving Generic
data IVec3 = IVec3 !Int32 !Int32 !Int32 deriving Generic
data IVec4 = IVec4 !Int32 !Int32 !Int32 !Int32 deriving Generic

-- TODO: gvec expr

infixl 7 .*.
infixl 7 .*
infixr 7 *.

class Matrix a where
        type family Row a = b | b -> a
        idmtx :: a
        transpose :: a -> a
        (.*.) :: a -> a -> a
        (.*) :: a -> Row a -> Row a
        (*.) :: Row a -> a -> Row a
        v *. m = transpose m .* v

instance Matrix Mat2
instance Matrix Mat3


-- ? Num/Fractional/Floating instances?

instance AdditiveGroup Vec2 where
        zeroV = Vec2 0 0
        (^+^) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
        (^-^) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
        negateV (Vec2 x y) = Vec2 (-x) (-y)

instance VectorSpace Vec2 where
        type Scalar Vec2 = Float
        (*^) s (Vec2 x y) = Vec2 (s * x) (s * y)

instance InnerSpace Vec2 where
        (<.>) (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

instance Storable Vec2 where
        sizeOf _ = 8
        alignment _ = 4
        peek ptr = Vec2 <$> peekElemOff (castPtr ptr) 0
                        <*> peekElemOff (castPtr ptr) 1
        poke ptr (Vec2 x y) = do pokeElemOff (castPtr ptr) 0 x
                                 pokeElemOff (castPtr ptr) 1 y


instance AdditiveGroup Vec3 where
        zeroV = Vec3 0 0 0
        (^+^) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
                Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
        (^-^) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
                Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
        negateV (Vec3 x y z) = Vec3 (-x) (-y) (-z)

instance VectorSpace Vec3 where
        type Scalar Vec3 = Float
        (*^) s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)

instance InnerSpace Vec3 where
        (<.>) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance HasCross3 Vec3 where
        cross3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
                Vec3 (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1)

instance Storable Vec3 where
        sizeOf _ = 12
        alignment _ = 4
        peek ptr = Vec3 <$> peekElemOff (castPtr ptr) 0
                        <*> peekElemOff (castPtr ptr) 1
                        <*> peekElemOff (castPtr ptr) 2
        poke ptr (Vec3 x y z) = do pokeElemOff (castPtr ptr) 0 x
                                   pokeElemOff (castPtr ptr) 1 y
                                   pokeElemOff (castPtr ptr) 2 z


instance AdditiveGroup Vec4 where
        zeroV = Vec4 0 0 0 0
        (^+^) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
                Vec4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
        (^-^) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
                Vec4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
        negateV (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)

instance VectorSpace Vec4 where
        type Scalar Vec4 = Float
        (*^) s (Vec4 x y z w) = Vec4 (s * x) (s * y) (s * z) (s * w)

instance InnerSpace Vec4 where
        (<.>) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
                x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2

instance Storable Vec4 where
        sizeOf _ = 16
        alignment _ = 4
        peek ptr = Vec4 <$> peekElemOff (castPtr ptr) 0
                        <*> peekElemOff (castPtr ptr) 1
                        <*> peekElemOff (castPtr ptr) 2
                        <*> peekElemOff (castPtr ptr) 3
        poke ptr (Vec4 x y z w) = do pokeElemOff (castPtr ptr) 0 x
                                     pokeElemOff (castPtr ptr) 1 y
                                     pokeElemOff (castPtr ptr) 2 z
                                     pokeElemOff (castPtr ptr) 3 w

-- TODO: additivegroup, vectorspace, mat2, mat3
instance Matrix Mat4 where
        type Row Mat4 = Vec4
        idmtx = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)
        transpose (Mat4 (Vec4 a b c d)
                        (Vec4 e f g h)
                        (Vec4 i j k l)
                        (Vec4 m n o p)) = Mat4 (Vec4 a e i m)
                                               (Vec4 b f j n)
                                               (Vec4 c g k o)
                                               (Vec4 d h l p)
        (.*.) (Mat4 (Vec4 _1 _2 _3 _4)
                    (Vec4 _5 _6 _7 _8)
                    (Vec4 _9 _a _b _c)
                    (Vec4 _d _e _f _g))
              (Mat4 (Vec4 a b c d)
                    (Vec4 e f g h)
                    (Vec4 i j k l)
                    (Vec4 m n o p)) =
                Mat4 (Vec4 (_1 * a + _2 * e + _3 * i + _4 * m)
                           (_1 * b + _2 * f + _3 * j + _4 * n)
                           (_1 * c + _2 * g + _3 * k + _4 * o)
                           (_1 * d + _2 * h + _3 * l + _4 * p))
        
                     (Vec4 (_5 * a + _6 * e + _7 * i + _8 * m)
                           (_5 * b + _6 * f + _7 * j + _8 * n)
                           (_5 * c + _6 * g + _7 * k + _8 * o)
                           (_5 * d + _6 * h + _7 * l + _8 * p))
        
                     (Vec4 (_9 * a + _a * e + _b * i + _c * m)
                           (_9 * b + _a * f + _b * j + _c * n)
                           (_9 * c + _a * g + _b * k + _c * o)
                           (_9 * d + _a * h + _b * l + _c * p))
        
                     (Vec4 (_d * a + _e * e + _f * i + _g * m)
                           (_d * b + _e * f + _f * j + _g * n)
                           (_d * c + _e * g + _f * k + _g * o)
                           (_d * d + _e * h + _f * l + _g * p))

        (.*) (Mat4 (Vec4 a b c d)
                   (Vec4 e f g h)
                   (Vec4 i j k l)
                   (Vec4 m n o p))
             (Vec4 x y z w) = Vec4 (a * x + b * y + c * z + d * w)
                                   (e * x + f * y + g * z + h * w)
                                   (i * x + j * y + k * z + l * w)
                                   (m * x + n * y + o * z + p * w)


instance Storable IVec2 where
        sizeOf _ = 8
        alignment _ = 4
        peek ptr = IVec2 <$> peekElemOff (castPtr ptr) 0
                         <*> peekElemOff (castPtr ptr) 1
        poke ptr (IVec2 x y) = do pokeElemOff (castPtr ptr) 0 x
                                  pokeElemOff (castPtr ptr) 1 y

instance Storable IVec3 where
        sizeOf _ = 12
        alignment _ = 4
        peek ptr = IVec3 <$> peekElemOff (castPtr ptr) 0
                         <*> peekElemOff (castPtr ptr) 1
                         <*> peekElemOff (castPtr ptr) 2
        poke ptr (IVec3 x y z) = do pokeElemOff (castPtr ptr) 0 x
                                    pokeElemOff (castPtr ptr) 1 y
                                    pokeElemOff (castPtr ptr) 2 z

instance Storable IVec4 where
        sizeOf _ = 16
        alignment _ = 4
        peek ptr = IVec4 <$> peekElemOff (castPtr ptr) 0
                         <*> peekElemOff (castPtr ptr) 1
                         <*> peekElemOff (castPtr ptr) 2
                         <*> peekElemOff (castPtr ptr) 3
        poke ptr (IVec4 x y z w) = do pokeElemOff (castPtr ptr) 0 x
                                      pokeElemOff (castPtr ptr) 1 y
                                      pokeElemOff (castPtr ptr) 2 z
                                      pokeElemOff (castPtr ptr) 3 w

instance Hashable Vec2
instance Hashable Vec3
instance Hashable Vec4
instance Hashable Mat2
instance Hashable Mat3
instance Hashable Mat4
instance Hashable IVec2
instance Hashable IVec3
instance Hashable IVec4
