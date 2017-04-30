{-# LANGUAGE DataKinds, DeriveGeneric, GADTs #-}

-- A program consists of a vertex shader and a fragment shader.
module Program3D (
        module Graphics.Rendering.Ombra.Shader.Default3D,
        Uniforms,
        fragmentShader
) where

import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Default3D
       hiding (fragmentShader, Uniforms)

import ProgramCommon

type Uniforms = '[ Time, NoiseTexture, Project3, View3, Transform3, Texture2 ]

fragmentShader :: FragmentShader '[ Time, NoiseTexture, Texture2 ]
                                 '[ Position3, UV, Normal3 ]
fragmentShader (Time time :-
                NoiseTexture noiseSampler :-
                Texture2 sampler :- N)
               (_ :- UV (GVec2 s t) :- _)
                = let GVec4 value _ _ _ = texture2D noiseSampler $ GVec2 s t
                      intensity = fract $ value + time
                      color = GVec4 0.5 0.6 0 1
                      texColor = texture2D sampler $ GVec2 s (1 - t)
                  in Fragment (mix texColor color intensity) :- N
