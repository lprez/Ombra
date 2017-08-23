-- |
-- Module:      Graphics.Rendering.Ombra.Stencil
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- The stencil test lets you manipulate the stencil buffer and discard fragments
-- on the basis of the result of the operation.

module Graphics.Rendering.Ombra.Stencil (
        Mode(..),
        Side(..),
        Function(..),
        FunctionType(..),
        Operation(..),
        OperationType(..),
        withStencilMode
) where

import Graphics.Rendering.Ombra.Stencil.Draw
import Graphics.Rendering.Ombra.Stencil.Types
