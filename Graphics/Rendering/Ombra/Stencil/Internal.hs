module Graphics.Rendering.Ombra.Stencil.Internal where

import Graphics.Rendering.Ombra.Internal.GL

-- | Stencil mode.
data Mode = Mode (Side Function) (Side Operation) deriving Eq

data Side a = FrontBack a       -- ^ Use the same value for both sides.
            | Separate a a      -- ^ Use a different value for each side (front
                                -- and back).
            deriving Eq

-- | Function type, fragment stencil value and mask.
data Function = Function FunctionType Int Word deriving Eq

-- | Operation to perform between the masked fragment stencil value and the
-- masked destination stencil value.
data FunctionType = Never               -- ^ Never pass.
                  | Always              -- ^ Always pass.
                  | Less                -- ^ <
                  | LessOrEqual         -- ^ <=
                  | Greater             -- ^ \>
                  | GreaterOrEqual      -- ^ \>=
                  | Equal               -- ^ ==
                  | NotEqual            -- ^ /=
                  deriving Eq

-- | Operations to perform if the stencil test fails, if the stencil test passes
-- but the depth test fails, and if both the stencil test and the depth test
-- pass.
data Operation = Operation OperationType OperationType OperationType deriving Eq

-- | Operation to perform to the stencil value in the buffer.
data OperationType = Keep       -- ^ Keep it unchanged.
                   | Invert     -- ^ Invert it.
                   | Zero       -- ^ Set it to zero.
                   | Replace    -- ^ Replace it with the masked fragment value.
                   | Increment  -- ^ Increment it if not maximum.
                   | Decrement  -- ^ Decrement it if not zero.
                   | IncWrap    -- ^ Increment it, wrapping it if it would
                                -- overflow.
                   | DecWrap    -- ^ Decrement it, wrapping it if it would
                                -- underflow.
                   deriving Eq

function :: GLES => Function -> (GLEnum, GLInt, GLUInt)
function (Function ty value mask) = ( getType ty
                                    , fromIntegral value
                                    , fromIntegral mask )
        where getType Never = gl_NEVER
              getType Always = gl_ALWAYS
              getType Less = gl_LESS
              getType LessOrEqual = gl_LEQUAL
              getType Greater = gl_GREATER
              getType GreaterOrEqual = gl_GEQUAL
              getType Equal = gl_EQUAL
              getType NotEqual = gl_NOTEQUAL

operation :: GLES => Operation -> (GLEnum, GLEnum, GLEnum)
operation (Operation sf spdf spdp) = (getOp sf, getOp spdf, getOp spdp)
        where getOp Keep = gl_KEEP
              getOp Invert = gl_INVERT
              getOp Zero = gl_ZERO
              getOp Replace = gl_REPLACE
              getOp Increment = gl_INCR
              getOp Decrement = gl_DECR
              getOp IncWrap = gl_INCR_WRAP
              getOp DecWrap = gl_DECR_WRAP
