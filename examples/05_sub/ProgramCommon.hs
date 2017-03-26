{-# LANGUAGE DeriveGeneric #-}

module ProgramCommon (NoiseTexture(..), Time(..)) where

import Graphics.Rendering.Ombra.Shader
import Prelude ()

data NoiseTexture = NoiseTexture GSampler2D deriving Generic
data Time = Time GFloat deriving Generic
