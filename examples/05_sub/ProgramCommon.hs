{-# LANGUAGE DeriveGeneric #-}

module ProgramCommon (NoiseTexture(..), Time(..)) where

import Graphics.Rendering.Ombra.Shader
import Prelude ()

data NoiseTexture = NoiseTexture Sampler2D deriving Generic
data Time = Time Float deriving Generic
