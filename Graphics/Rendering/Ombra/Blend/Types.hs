module Graphics.Rendering.Ombra.Blend.Types where

import Graphics.Rendering.Ombra.Vector

-- | Blend mode.
data Mode = Mode {
        -- | The color that will be used if you choose the 'ConstantColor' or
        -- 'ConstantAlpha' parameter.
        constantColor :: Maybe Vec4,
        -- | Operation to apply to the colors (first three components of the
        -- color vectors).
        rgbOperator :: Operator,
        -- | Multipliers of the source color and destination color,
        -- respectively.
        rgbParameters :: Maybe (Parameter, Parameter),
        -- | Operation to apply to the alpha component.
        alphaOperator :: Operator,
        -- | Multipliers of the source alpha and destination alpha.
        alphaParameters :: Maybe (Parameter, Parameter)
} deriving (Eq, Ord)

-- | Blend operator.
data Operator = Add | Subtract | ReverseSubtract {- Min | Max -}
        deriving (Eq, Ord)

-- | Blend function parameters.
data Parameter = Zero | One | SourceColor | DestinationColor | ConstantColor
               | SourceAlpha | DestinationAlpha | ConstantAlpha
               | OneMinus Parameter
               deriving (Eq, Ord)

