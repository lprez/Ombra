{-# LANGUAGE DataKinds, DeriveGeneric, GADTs #-}

module ProgramDistort (
        module Graphics.Rendering.Ombra.Shader.Default2D,
        Uniforms,
        fragmentShader
) where

import Graphics.Rendering.Ombra.Shader

-- We'll use the standard 2D program to apply an effect on the rendered scene,
-- adding two uniforms (NoiseTexture and Time) and modifying the fragment
-- shader.
import Graphics.Rendering.Ombra.Shader.Default2D
        hiding (fragmentShader, Uniforms)

import ProgramCommon

type Uniforms = '[View2, Time, NoiseTexture, Image, Depth, Transform2]

fragmentShader :: FragmentShader '[Time, NoiseTexture, Image] '[UV]
fragmentShader (Time time :- NoiseTexture noise :- Image sampler :- N)
               (UV (GVec2 s t) :- N) =
                let -- You should use the 'store' function when you use the
                    -- same variable more than one time, to avoid recalculating
                    -- it at every occurrence.
                    GVec4 value _ _ _ = store $
                                        texture2D noise
                                                  (GVec2 s (fract $ t + time))

                    -- Functions in the shaders are analogous to macros.
                    dist :: GFloat -> GFloat -> GFloat
                    dist x fac = fract $ x + value / fac

                    texCol = texture2D sampler $ GVec2 (dist s 10) (dist t 20)
                in Fragment texCol :- N
