{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, MultiParamTypeClasses,
             TypeFamilyDependencies, TypeFamilies, FlexibleInstances,
             UndecidableInstances, DeriveGeneric #-}

{-|
Module:      Graphics.Rendering.Ombra.Shader.Language
License:     BSD3
Maintainer:  ziocroc@gmail.com
Stability:   experimental
Portability: GHC only

This module exports the shader EDSL.
-}

module Graphics.Rendering.Ombra.Shader.Language (
        module Data.Boolean,
        module Data.VectorSpace,
        module Data.Cross,
        -- * Types
        -- ** GPU types
        Shader.GBool,
        Shader.GFloat,
        Shader.GInt,
        Shader.GSampler2D,
        -- Shader.GSamplerCube,
        Shader.GVec2(..),
        Shader.GVec3(..),
        Shader.GVec4(..),
        Shader.GBVec2(..),
        Shader.GBVec3(..),
        Shader.GBVec4(..),
        Shader.GIVec2(..),
        Shader.GIVec3(..),
        Shader.GIVec4(..),
        Shader.GMat2(..),
        Shader.GMat3(..),
        Shader.GMat4(..),
        Shader.GenType,
        Shader.GenTypeGFloat,
        Shader.GArray,
        -- * GPU functions
        (Shader.!),
        Shader.loop,
        Shader.store,
        sample,
        {-
        Shader.texture2DBias,
        Shader.texture2DProj,
        Shader.texture2DProjBias,
        Shader.texture2DProj4,
        Shader.texture2DProjBias4,
        Shader.texture2DLod,
        Shader.texture2DProjLod,
        Shader.texture2DProjLod4,
        -}
        Shader.arrayLength,
        -- ** Various math functions
        Matrix(..),
        Ext(..),
        minG,
        maxG,
        modG,
        floorG,
        ceilingG,
        Shader.radians,
        Shader.degrees,
        Shader.exp2,
        Shader.log2,
        Shader.inversesqrt,
        Shader.fract,
        Shader.clamp,
        Shader.mix,
        Shader.step,
        -- Shader.smoothstep,
        Shader.distance, -- TODO: implement AffineSpace?
        -- Shader.length,
        Shader.faceforward,
        Shader.reflect,
        Shader.refract,
        Shader.matrixCompMult,
        -- *** Partial derivatives
        -- | These are available only in the fragment shader.
        -- *** Vector relational functions
        Shader.VecOrd,
        Shader.VecEq,
        Shader.lessThan,
        Shader.lessThanEqual,
        Shader.greaterThan,
        Shader.greaterThanEqual,
        Shader.equal,
        Shader.notEqual,
        Shader.GBoolVector,
        Shader.anyBV,
        Shader.allBV,
        Shader.notBV,
        -- ** Constructors
        Shader.ToGBool,
        Shader.bool,
        Shader.ToGInt,
        Shader.int,
        Shader.ToGFloat,
        Shader.float,
        -- TODO: better vector constructors
        {-
        Shader.Components,
        Shader.CompList,
        Shader.ToCompList,
        (Shader.#),
        Shader.ToGVec2,
        Shader.vec2,
        Shader.ToGVec3,
        Shader.vec3,
        Shader.ToGVec4,
        Shader.vec4,
        Shader.ToGBVec2,
        Shader.bvec2,
        Shader.ToGBVec3,
        Shader.bvec3,
        Shader.ToGBVec4,
        Shader.bvec4,
        Shader.ToGIVec2,
        Shader.ivec2,
        Shader.ToGIVec3,
        Shader.ivec3,
        Shader.ToGIVec4,
        Shader.ivec4,
        Shader.ToGMat2,
        Shader.mat2,
        Shader.ToGMat3,
        Shader.mat3,
        Shader.ToGMat4,
        Shader.mat4,
        -}
        -- ** Other
) where

import Data.Proxy
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language.Functions ((#))
import qualified Graphics.Rendering.Ombra.Shader.Language.Types as Shader
import qualified Graphics.Rendering.Ombra.Shader.Language.Functions as Shader
import Graphics.Rendering.Ombra.Vector

import Data.Boolean
import qualified Data.Boolean.Numbers as B
import Data.Cross
import Data.VectorSpace

type instance BooleanOf Shader.GBool = Shader.GBool
type instance BooleanOf Shader.GFloat = Shader.GBool
type instance BooleanOf Shader.GInt = Shader.GBool
type instance BooleanOf Shader.GSampler2D = Shader.GBool
-- type instance BooleanOf Shader.GSamplerCube = Shader.GBool
type instance BooleanOf Shader.GVec2 = Shader.GBool
type instance BooleanOf Shader.GVec3 = Shader.GBool
type instance BooleanOf Shader.GVec4 = Shader.GBool
type instance BooleanOf Shader.GBVec2 = Shader.GBool
type instance BooleanOf Shader.GBVec3 = Shader.GBool
type instance BooleanOf Shader.GBVec4 = Shader.GBool
type instance BooleanOf Shader.GIVec2 = Shader.GBool
type instance BooleanOf Shader.GIVec3 = Shader.GBool
type instance BooleanOf Shader.GIVec4 = Shader.GBool
type instance BooleanOf Shader.GMat2 = Shader.GBool
type instance BooleanOf Shader.GMat3 = Shader.GBool
type instance BooleanOf Shader.GMat4 = Shader.GBool
type instance BooleanOf (Shader.GArray n t) = Shader.GBool

instance Boolean Shader.GBool where
        true = Shader.true
        false = Shader.false
        (&&*) = (Shader.&&)
        (||*) = (Shader.||)
        notB = Shader.not

instance (Shader.ShaderType a, BooleanOf a ~ Shader.GBool) => IfB a where
        ifB = Shader.ifThenElse

instance (Shader.ShaderType a, BooleanOf a ~ Shader.GBool) => EqB a where
        (==*) = (Shader.==)
        (/=*) = (Shader./=)

instance (Shader.ShaderType a, BooleanOf a ~ Shader.GBool) => OrdB a where
        (<*) = (Shader.<)
        (<=*) = (Shader.<=)
        (>*) = (Shader.>)
        (>=*) = (Shader.>=)
        
-- | Faster GPU 'max'/'B.maxB'.
maxG :: Shader.GenTypeGFloat a b => a -> b -> a
maxG = Shader.max

-- | Faster GPU 'min'/'B.minB'.
minG :: Shader.GenTypeGFloat a b => a -> b -> a
minG = Shader.min

instance Num Shader.GFloat where
        (+) = (Shader.+)
        (-) = (Shader.-)
        (*) = (Shader.*)
        abs = Shader.abs
        signum = Shader.sign
        fromInteger = Shader.fromInteger
        negate = Shader.negate

instance Num Shader.GInt where
        (+) = (Shader.+)
        (-) = (Shader.-)
        (*) = (Shader.*)
        abs = Shader.absI
        signum = Shader.signI
        fromInteger = Shader.fromInteger
        negate = Shader.negateI

instance B.NumB Shader.GFloat where
        type IntegerOf Shader.GFloat = Shader.GInt
        fromIntegerB = Shader.float

instance B.NumB Shader.GInt where
        type IntegerOf Shader.GInt = Shader.GInt
        fromIntegerB = id

instance AdditiveGroup Shader.GFloat where
        zeroV = Shader.zero
        (^+^) = (Shader.+)
        (^-^) = (Shader.-)
        negateV = Shader.negate

-- | GPU 'mod' that can be used on floats and float vectors.
modG :: Shader.GenType a => a -> a -> a
modG = Shader.mod

instance B.IntegralB Shader.GInt where
        quotRem a b = let q = a Shader./ b in (q, a - b * q)
        -- XXX: ???
        divMod a b = let (q, r) = B.quotRem a b
                         f = 1 - abs (signum r + signum b)
                     in (q - f, r + b * f)
        toIntegerB = id

instance Fractional Shader.GFloat where
        (/) = (Shader./)
        fromRational = Shader.fromRational

instance Floating Shader.GFloat where
        pi = 3.1415926535897932384626433832795
        exp = Shader.exp
        log = Shader.log
        sqrt = Shader.sqrt
        (**) = (Shader.^)
        sin = Shader.sin
        cos = Shader.cos
        tan = Shader.tan
        asin = Shader.asin
        acos = Shader.acos
        atan = Shader.atan
        -- TODO
        sinh = error "Hyperbolic functions are not implemented."
        cosh = error "Hyperbolic functions are not implemented."
        asinh = error "Hyperbolic functions are not implemented."
        acosh = error "Hyperbolic functions are not implemented."
        atanh = error "Hyperbolic functions are not implemented."

floatToInt :: (B.NumB b, B.IntegerOf b ~ Shader.GInt) => Shader.GFloat -> b
floatToInt = B.fromIntegerB . Shader.int

instance B.RealFracB Shader.GFloat where
        properFraction x = let tx = signum x * floorG (abs x)
                           in (floatToInt tx, x - tx)
        -- truncate x = floatToInt $ signum x * floorG (abs x)
        round x = floatToInt . floorG $ x + 0.5
        ceiling = floatToInt . ceilingG
        floor = floatToInt . floorG

floorG :: Shader.GenType a => a -> a
floorG = Shader.floor

ceilingG :: Shader.GenType a => a -> a
ceilingG = Shader.ceil

instance B.RealFloatB Shader.GFloat where
        isNaN = error "isNaN: not supported"
        isInfinite = error "isInfinite: not supported"
        isNegativeZero = error "isNegativeZero: not supported"
        isIEEE = error "isIEEE: not supported"
        atan2 = Shader.atan2

-- Vectors

instance AdditiveGroup Shader.GVec2 where
        zeroV = Shader.zero
        (^+^) = (Shader.+)
        (^-^) = (Shader.-)
        negateV = Shader.negate

instance VectorSpace Shader.GVec2 where
        type Scalar Shader.GVec2 = Shader.GFloat
        (*^) = (Shader.*)

instance InnerSpace Shader.GVec2 where
        (<.>) = Shader.dot

instance Ext Shader.GVec2 where
        type Extended Shader.GVec2 = Shader.GVec3
        v ^| z = Shader.vec3 $ v # z
        v ^|^ Shader.GVec3 _ _ z = Shader.vec3 $ v # z
        extract = Shader.vec2

instance AdditiveGroup Shader.GVec3 where
        zeroV = Shader.zero
        (^+^) = (Shader.+)
        (^-^) = (Shader.-)
        negateV = Shader.negate

instance VectorSpace Shader.GVec3 where
        type Scalar Shader.GVec3 = Shader.GFloat
        (*^) = (Shader.*)

instance InnerSpace Shader.GVec3 where
        (<.>) = Shader.dot

instance HasCross3 Shader.GVec3 where
        cross3 = Shader.cross

instance Ext Shader.GVec3 where
        type Extended Shader.GVec3 = Shader.GVec4
        v ^| w = Shader.vec4 $ v # w
        v ^|^ Shader.GVec4 _ _ _ w = Shader.vec4 $ v # w
        extract = Shader.vec3

instance AdditiveGroup Shader.GVec4 where
        zeroV = Shader.zero
        (^+^) = (Shader.+)
        (^-^) = (Shader.-)
        negateV = Shader.negate

instance VectorSpace Shader.GVec4 where
        type Scalar Shader.GVec4 = Shader.GFloat
        (*^) = (Shader.*)

instance InnerSpace Shader.GVec4 where
        (<.>) = Shader.dot

-- Matrices

instance AdditiveGroup Shader.GMat2 where
        zeroV = Shader.zero
        (^+^) = (Shader.+)
        (^-^) = (Shader.-)
        negateV = Shader.negateM

instance VectorSpace Shader.GMat2 where
        type Scalar Shader.GMat2 = Shader.GFloat
        (*^) = (Shader.*)

instance AdditiveGroup Shader.GMat3 where
        zeroV = Shader.zero
        (^+^) = (Shader.+)
        (^-^) = (Shader.-)
        negateV = Shader.negateM

instance Ext Shader.GMat2 where
        type Extended Shader.GMat2 = Shader.GMat3
        v ^| z = Shader.mat3 $ v # z # z # z # z # z
        Shader.GMat2 x y ^|^ Shader.GMat3 x' y' z' =
                Shader.GMat3 (x ^|^ x') (y ^|^ y') z'
        extract = Shader.mat2

instance Matrix Shader.GMat2 where
        type Row Shader.GMat2 = Shader.GVec2
        idmtx = Shader.mat2 (1.0 :: Shader.GFloat)
        (.*.) = (Shader.*)
        (.*) = (Shader.*)
        (*.) = (Shader.*)
        transpose (Shader.GMat2 (Shader.GVec2 a b) (Shader.GVec2 c d)) =
                Shader.GMat2 (Shader.GVec2 a c) (Shader.GVec2 b d)

instance VectorSpace Shader.GMat3 where
        type Scalar Shader.GMat3 = Shader.GFloat
        (*^) = (Shader.*)

instance Ext Shader.GMat3 where
        type Extended Shader.GMat3 = Shader.GMat4
        v ^| z = Shader.mat4 $ v # z # z # z # z # z # z # z
        Shader.GMat3 x y z ^|^ Shader.GMat4 x' y' z' w' =
                Shader.GMat4 (x ^|^ x') (y ^|^ y') (z ^|^ z') w'
        extract = Shader.mat3

instance Matrix Shader.GMat3 where
        type Row Shader.GMat3 = Shader.GVec3
        idmtx = Shader.mat3 (1.0 :: Shader.GFloat)
        (.*.) = (Shader.*)
        (.*) = (Shader.*)
        (*.) = (Shader.*)
        transpose (Shader.GMat3 (Shader.GVec3 a b c)
                                (Shader.GVec3 d e f)
                                (Shader.GVec3 g h i)) =
                        Shader.GMat3 (Shader.GVec3 a d g)
                                     (Shader.GVec3 b e h)
                                     (Shader.GVec3 c f i)

instance AdditiveGroup Shader.GMat4 where
        zeroV = Shader.zero
        (^+^) = (Shader.+)
        (^-^) = (Shader.-)
        negateV = Shader.negateM

instance VectorSpace Shader.GMat4 where
        type Scalar Shader.GMat4 = Shader.GFloat
        (*^) = (Shader.*)

instance Matrix Shader.GMat4 where
        type Row Shader.GMat4 = Shader.GVec4
        idmtx = Shader.mat4 (1.0 :: Shader.GFloat)
        (.*.) = (Shader.*)
        (.*) = (Shader.*)
        (*.) = (Shader.*)
        transpose (Shader.GMat4 (Shader.GVec4 a b c d)
                                (Shader.GVec4 e f g h)
                                (Shader.GVec4 i j k l)
                                (Shader.GVec4 m n o p)) =
                        Shader.GMat4 (Shader.GVec4 a e i m)
                                     (Shader.GVec4 b f j n)
                                     (Shader.GVec4 c g k o)
                                     (Shader.GVec4 d h l p)

-- | Sample a texel from a texture.
sample :: Shader.GSampler2D -> Shader.GVec2 -> Shader.GVec4
sample = Shader.texture2D
