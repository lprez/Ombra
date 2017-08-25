-- |
-- Module:      Graphics.Rendering.Ombra.Blend
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Blending lets you combine the colors in the color buffer with the output of
-- the fragment shader, using a custom operation.

module Graphics.Rendering.Ombra.Blend (
        -- * Types
        Mode(..),
        Operator(..),
        Parameter(..),
        withBlendMode,

        -- * Blending modes
        transparency,
        additive,
) where

import Graphics.Rendering.Ombra.Blend.Draw
import Graphics.Rendering.Ombra.Blend.Types

-- | Standard transparency.
transparency :: Mode
transparency = Mode Nothing Add (Just (SourceAlpha, OneMinus SourceAlpha))
                            Add (Just (SourceAlpha, OneMinus SourceAlpha))

-- | Additive blend mode.
additive :: Mode
additive = Mode Nothing Add (Just (One, One)) Add (Just (One, One))
