{-# LANGUAGE DeriveGeneric, TypeFamilies, TypeFamilyDependencies #-}

-- |
-- Module:      Graphics.Rendering.Ombra.Vector
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: GHC only

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
        Matrix(..),
        Ext(..)
) where

import Data.Cross
import Data.VectorSpace
import Data.Hashable
import Data.Int
import Foreign.Storable
import Foreign.Ptr (castPtr)
import GHC.Generics

data Vec2 = Vec2 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 deriving (Generic, Show, Eq)
data Vec3 = Vec3 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 deriving (Generic, Show, Eq)
data Vec4 = Vec4 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 deriving (Generic, Show, Eq)

data Mat2 = Mat2 !Vec2 !Vec2 deriving (Generic, Show, Eq)
data Mat3 = Mat3 !Vec3 !Vec3 !Vec3 deriving (Generic, Show, Eq)
data Mat4 = Mat4 !Vec4 !Vec4 !Vec4 !Vec4 deriving (Generic, Show, Eq)

data IVec2 = IVec2 {-# UNPACK #-} !Int32
                   {-# UNPACK #-} !Int32
                   deriving (Generic, Show, Eq)
data IVec3 = IVec3 {-# UNPACK #-} !Int32
                   {-# UNPACK #-} !Int32
                   {-# UNPACK #-} !Int32
                   deriving (Generic, Show, Eq)
data IVec4 = IVec4 {-# UNPACK #-} !Int32
                   {-# UNPACK #-} !Int32
                   {-# UNPACK #-} !Int32
                   {-# UNPACK #-} !Int32
                   deriving (Generic, Show, Eq)

infixl 5 ^|
infixl 5 ^|^
class VectorSpace v => Ext v where
        type Extended v = w | w -> v
        -- | Extend the vector with a specified scalar.
        (^|) :: v -> Scalar v -> Extended v
        -- | Extend the first vector using the components of the second vector.
        --
        -- For instance:
        -- @
        -- Mat2 (Vec2 x y) (Vec2 z w) ^|^ idmtx =
        -- Mat3 (Vec3 x y 0) (Vec3 z w 0) (Vec3 0 0 1)
        -- @
        (^|^) :: v -> Extended v -> Extended v
        -- | Extract a smaller vector.
        extract :: Extended v -> v

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

-- TODO: ? Num/Fractional/Floating instances?

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

instance Ext Vec2 where
        type Extended Vec2 = Vec3
        Vec2 x y ^| z = Vec3 x y z
        Vec2 x y ^|^ Vec3 _ _ z = Vec3 x y z
        extract (Vec3 x y z) = Vec2 x y

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

instance Ext Vec3 where
        type Extended Vec3 = Vec4
        Vec3 x y z ^| w = Vec4 x y z w
        Vec3 x y z ^|^ Vec4 _ _ _ w = Vec4 x y z w
        extract (Vec4 x y z w) = Vec3 x y z

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

instance AdditiveGroup Mat2 where
        zeroV = Mat2 zeroV zeroV
        (^+^) (Mat2 x1 y1) (Mat2 x2 y2) = Mat2 (x1 ^+^ x2) (y1 ^+^ y2)
        (^-^) (Mat2 x1 y1) (Mat2 x2 y2) = Mat2 (x1 ^-^ x2) (y1 ^-^ y2)
        negateV (Mat2 x y) = Mat2 (negateV x) (negateV y)

instance VectorSpace Mat2 where
        type Scalar Mat2 = Float
        (*^) s (Mat2 x y) = Mat2 (s *^ x) (s *^ y)

instance Ext Mat2 where
        type Extended Mat2 = Mat3
        Mat2 x y ^| a = Mat3 (x ^| a) (y ^| a) (Vec3 a a a)
        Mat2 x y ^|^ Mat3 x' y' z' = Mat3 (x ^|^ x') (y ^|^ y') z'
        extract (Mat3 x y z) = Mat2 (extract x) (extract y)

instance Matrix Mat2 where
        type Row Mat2 = Vec2
        idmtx = Mat2 (Vec2 1 0) (Vec2 0 1)
        transpose (Mat2 (Vec2 a b) (Vec2 c d)) = Mat2 (Vec2 a c) (Vec2 b d)
        (.*.) (Mat2 (Vec2 a11 a12) (Vec2 a21 a22))
              (Mat2 (Vec2 b11 b12) (Vec2 b21 b22)) =
                Mat2 (Vec2 (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22))
                     (Vec2 (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22))

        (.*) (Mat2 (Vec2 a b) (Vec2 c d)) (Vec2 x y) =
                Vec2 (a * x + b * y) (c * x + d * y)


instance AdditiveGroup Mat3 where
        zeroV = Mat3 zeroV zeroV zeroV
        (^+^) (Mat3 x1 y1 z1) (Mat3 x2 y2 z2) =
                Mat3 (x1 ^+^ x2) (y1 ^+^ y2) (z1 ^+^ z2)
        (^-^) (Mat3 x1 y1 z1) (Mat3 x2 y2 z2) =
                Mat3 (x1 ^-^ x2) (y1 ^-^ y2) (z1 ^-^ z2)
        negateV (Mat3 x y z) = Mat3 (negateV x) (negateV y) (negateV z)

instance VectorSpace Mat3 where
        type Scalar Mat3 = Float
        (*^) s (Mat3 x y z) = Mat3 (s *^ x) (s *^ y) (s *^ z)

instance Ext Mat3 where
        type Extended Mat3 = Mat4
        Mat3 x y z ^| a = Mat4 (x ^| a) (y ^| a) (z ^| a) (Vec4 a a a a)
        Mat3 x y z ^|^ Mat4 x' y' z' w' = Mat4 (x ^|^ x') (y ^|^ y')
                                               (z ^|^ z') w'
        extract (Mat4 x y z w) = Mat3 (extract x) (extract y) (extract z)

instance Matrix Mat3 where
        type Row Mat3 = Vec3
        idmtx = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)
        transpose (Mat3 (Vec3 a b c)
                        (Vec3 d e f)
                        (Vec3 g h i)) = Mat3 (Vec3 a d g)
                                             (Vec3 b e h)
                                             (Vec3 c f i)
        (.*.) (Mat3 (Vec3 a11 a12 a13)
                    (Vec3 a21 a22 a23)
                    (Vec3 a31 a32 a33))
              (Mat3 (Vec3 b11 b12 b13)
                    (Vec3 b21 b22 b23)
                    (Vec3 b31 b32 b33)) =
                Mat3 (Vec3 (a11 * b11 + a12 * b21 + a13 * b31)
                           (a11 * b12 + a12 * b22 + a13 * b32)
                           (a11 * b13 + a12 * b23 + a13 * b33))

                     (Vec3 (a21 * b11 + a22 * b21 + a23 * b31)
                           (a21 * b12 + a22 * b22 + a23 * b32)
                           (a21 * b13 + a22 * b23 + a23 * b33))

                     (Vec3 (a31 * b11 + a32 * b21 + a33 * b31)
                           (a31 * b12 + a32 * b22 + a33 * b32)
                           (a31 * b13 + a32 * b23 + a33 * b33))

        (.*) (Mat3 (Vec3 a b c)
                   (Vec3 d e f)
                   (Vec3 g h i))
             (Vec3 x y z) = Vec3 (a * x + b * y + c * z)
                                 (d * x + e * y + f * z)
                                 (g * x + h * y + i * z)


instance AdditiveGroup Mat4 where
        zeroV = Mat4 zeroV zeroV zeroV zeroV
        (^+^) (Mat4 x1 y1 z1 w1) (Mat4 x2 y2 z2 w2) =
                Mat4 (x1 ^+^ x2) (y1 ^+^ y2) (z1 ^+^ z2) (w1 ^+^ w2)
        (^-^) (Mat4 x1 y1 z1 w1) (Mat4 x2 y2 z2 w2) =
                Mat4 (x1 ^-^ x2) (y1 ^-^ y2) (z1 ^-^ z2) (w1 ^-^ w2)
        negateV (Mat4 x y z w) = Mat4 (negateV x) (negateV y)
                                      (negateV z) (negateV w)

instance VectorSpace Mat4 where
        type Scalar Mat4 = Float
        (*^) s (Mat4 x y z w) = Mat4 (s *^ x) (s *^ y) (s *^ z) (s *^ w)

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

-- TODO: ??

instance Hashable Vec2 where
        hashWithSalt s (Vec2 x y) = hashWithSalt s (x, y)

instance Hashable Vec3 where
        hashWithSalt s (Vec3 x y z) = hashWithSalt s (x, y, z)

instance Hashable Vec4 where
        hashWithSalt s (Vec4 x y z w) = hashWithSalt s (x, y, z, w)

instance Hashable Mat2 where
        hashWithSalt s (Mat2 x y) = hashWithSalt s (x, y)

instance Hashable Mat3 where
        hashWithSalt s (Mat3 x y z) = hashWithSalt s (x, y, z)

instance Hashable Mat4 where
        hashWithSalt s (Mat4 x y z w) = hashWithSalt s (x, y, z, w)

instance Hashable IVec2 where
        hashWithSalt s (IVec2 x y) = hashWithSalt s (x, y)

instance Hashable IVec3 where
        hashWithSalt s (IVec3 x y z) = hashWithSalt s (x, y, z)

instance Hashable IVec4 where
        hashWithSalt s (IVec4 x y z w) = hashWithSalt s (x, y, z, w)
