{-# LANGUAGE DataKinds, RebindableSyntax, DeriveGeneric, GADTs #-}

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
               (UV (Vec2 s t) :- N) =
                let -- You should use the 'store' function when you use the
                    -- same variable more than one time, to avoid recalculating
                    -- it at every occurrence.
                    Vec4 value _ _ _ = store $
                                        texture2D noise
                                                  (Vec2 s (fract $ t + time))

                    -- Functions in the shaders are analogous to macros.
                    dist :: Float -> Float -> Float
                    dist x fac = fract $ x + value / fac

                    texCol = texture2D sampler $ Vec2 (dist s 10) (dist t 20)
                in Fragment texCol :- N
