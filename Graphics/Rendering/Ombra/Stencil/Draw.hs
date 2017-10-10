module Graphics.Rendering.Ombra.Stencil.Draw (
        MonadStencil(..),
        function,
        operation
) where

import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Stencil.Types

class (GLES, MonadGL m) => MonadStencil m where
        withStencilMode :: Maybe Mode -> m a -> m a

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
