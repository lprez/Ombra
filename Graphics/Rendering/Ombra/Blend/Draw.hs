module Graphics.Rendering.Ombra.Blend.Draw (
        MonadBlend(..),
        equation,
        function
) where

import Graphics.Rendering.Ombra.Blend.Types
import Graphics.Rendering.Ombra.Internal.GL

class (GLES, MonadGL m) => MonadBlend m where
        withBlendMode :: Maybe Mode -> m a -> m a

equation :: GLES => Mode -> (GLEnum, GLEnum)
equation m = (mode $ rgbOperator m, mode $ alphaOperator m)
        where mode Add = gl_FUNC_ADD
              mode Subtract = gl_FUNC_SUBTRACT
              mode ReverseSubtract = gl_FUNC_REVERSE_SUBTRACT
              -- mode Min = gl_MIN
              -- mode Max = gl_MAX

function :: GLES => Mode -> (GLEnum, GLEnum, GLEnum, GLEnum)
function m = (rgbs, rgbd, alphas, alphad)
        where (rgbs, rgbd) = case rgbParameters m of
                                  Just (p, p') -> (param p, param p')
                                  Nothing -> (gl_ZERO, gl_ZERO)
              (alphas, alphad) = case alphaParameters m of
                                      Just (p, p') -> (param p, param p')
                                      Nothing -> (gl_ZERO, gl_ZERO)
              param Zero = gl_ZERO
              param One = gl_ONE
              param SourceColor = gl_SRC_COLOR
              param DestinationColor = gl_DST_COLOR
              param ConstantColor = gl_CONSTANT_COLOR
              param SourceAlpha = gl_SRC_ALPHA
              param DestinationAlpha = gl_DST_ALPHA
              param ConstantAlpha = gl_CONSTANT_ALPHA
              param (OneMinus Zero) = gl_ONE
              param (OneMinus One) = gl_ZERO
              param (OneMinus SourceColor) = gl_ONE_MINUS_SRC_COLOR
              param (OneMinus DestinationColor) = gl_ONE_MINUS_DST_COLOR
              param (OneMinus ConstantColor) = gl_ONE_MINUS_CONSTANT_COLOR
              param (OneMinus SourceAlpha) = gl_ONE_MINUS_SRC_ALPHA
              param (OneMinus DestinationAlpha) = gl_ONE_MINUS_DST_ALPHA
              param (OneMinus ConstantAlpha) = gl_ONE_MINUS_CONSTANT_ALPHA
              param (OneMinus (OneMinus x)) = param x

