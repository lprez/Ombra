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

        -- * Blending modes
        transparency,
        additive
) where

import Graphics.Rendering.Ombra.Blend.Internal
