{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

{-|
An example of shader variable:

@
        data Transform2 = Transform2 Mat3 deriving Generic
@

An example of vertex shader:

@
        vertexShader :: VertexShader
        -- The types of the uniforms:
                                '[Transform2, View2, Depth]
        -- The types of the attributes:
                                '[Position2, UV]
        -- The types of the varying (outputs), excluding 'VertexShaderOutput'.
                                '[UV]
        vertexShader 
        -- Set of uniforms:
                     (Transform2 trans :- View2 view :- Depth z :- N)
        -- Set of attributes:
                     (Position2 (Vec2 x y) :- uv@(UV _) :- N) =
        -- Matrix and vector multiplication:
                        let Vec3 x' y' _ = view * trans * Vec3 x y 1
        -- Set of outputs:
                        in Vertex (Vec4 x' y' z 1) -- Vertex position.
                           :- uv :- N
@

Required extensions:

@
\{\-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, GADTs #\-\}
@

-}

-- TODO: Alternative version of the module that can be used without
-- RebindableSyntax and with Prelude functions. Use Num for scalars, vect
-- typeclasses for vectors and matrices, and prefixed functions for the other
-- clashing functions.

module Graphics.Rendering.Ombra.Shader (
        -- * Types
        Shader,
        VertexShader,
        FragmentShader,
        VertexShaderOutput(Vertex),
        FragmentShaderOutput(..),
        Valid,
        ValidVertex,
        NonDuplicate,
        Uniform,
        Attribute,
        Generic,
        SVList((:-), N),
        -- ** GPU types
        Bool,
        Float,
        Int,
        Sampler2D,
        SamplerCube,
        Vec2(..),
        Vec3(..),
        Vec4(..),
        BVec2(..),
        BVec3(..),
        BVec4(..),
        IVec2(..),
        IVec3(..),
        IVec4(..),
        Mat2(..),
        Mat3(..),
        Mat4(..),
        Array,
        -- * Functions
        loop,
        store,
        texture2D,
        texture2DBias,
        texture2DProj,
        texture2DProjBias,
        texture2DProj4,
        texture2DProjBias4,
        texture2DLod,
        texture2DProjLod,
        texture2DProjLod4,
        arrayLength,
        -- ** Math functions
        radians,
        degrees,
        sin,
        cos,
        tan,
        asin,
        acos,
        atan,
        atan2,
        exp,
        log,
        exp2,
        log2,
        sqrt,
        inversesqrt,
        abs,
        sign,
        floor,
        ceil,
        fract,
        mod,
        min,
        max,
        clamp,
        mix,
        step,
        smoothstep,
        length,
        distance,
        dot,
        cross,
        normalize,
        faceforward,
        reflect,
        refract,
        matrixCompMult,
        -- *** Vector relational functions
        VecOrd,
        VecEq,
        lessThan,
        lessThanEqual,
        greaterThan,
        greaterThanEqual,
        equal,
        notEqual,
        BoolVector,
        anyB,
        allB,
        notB,
        -- ** Constructors
        true,
        false,
        ToBool,
        bool,
        ToInt,
        int,
        ToFloat,
        float,
        Components,
        CompList,
        ToCompList,
        (#),
        ToVec2,
        vec2,
        ToVec3,
        vec3,
        ToVec4,
        vec4,
        ToBVec2,
        bvec2,
        ToBVec3,
        bvec3,
        ToBVec4,
        bvec4,
        ToIVec2,
        ivec2,
        ToIVec3,
        ivec3,
        ToIVec4,
        ivec4,
        ToMat2,
        mat2,
        ToMat3,
        mat3,
        ToMat4,
        mat4,
        -- ** Operators
        (*),
        (/),
        (+),
        (-),
        (^),
        (&&),
        (||),
        (==),
        (>=),
        (<=),
        (<),
        (>),
        (!),
        -- ** Rebinding functions
        fromInteger,
        fromRational,
        ifThenElse,
        negate,
        -- ** Prelude functions
        (.),
        id,
        const,
        flip,
        ($),
        CPU.fst,
        CPU.snd,
        -- * Variables
        position,
        fragData,
        fragCoord,
        fragFrontFacing
) where

import qualified Data.Int as CPU
import Data.Typeable (Typeable)
import qualified Data.Vect.Float as CPU
import GHC.Generics (Generic)
import qualified Graphics.Rendering.Ombra.Internal.GL as CPU
import qualified Graphics.Rendering.Ombra.Backend as CPU
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.Language.Functions
import Graphics.Rendering.Ombra.Shader.ShaderVar
import Graphics.Rendering.Ombra.Shader.Stages
import Prelude ((.), id, const, flip, ($))
import qualified Prelude as CPU
