{-# LANGUAGE DataKinds, DeriveGeneric, GADTs #-}

module Main where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.D3
import Graphics.Rendering.Ombra.Vector
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Default3D
       hiding (fragmentShader, Uniforms)
import Common

-- We'll pass two additional uniforms to the shaders (only the fragment shader
-- will use them).
data NoiseTexture = NoiseTexture GSampler2D deriving Generic
data Time = Time GFloat deriving Generic

type Uniforms = '[ Time, NoiseTexture, Project3, View3, Transform3, Texture2 ]

scene :: Texture -> Texture -> Float -> Layer
scene tex noiseTex time =
        let rotatedCube off =   trans (Vec3 0 (- 0.4) (- 1.6))
                              . rotY (time + off)
                              . trans (Vec3 0 0 0.8)
                              . rotY (time * 4)
                              . scale 0.2
                              $ cube tex

            viewGroup = viewPersp 0.5
                                  100000
                                  100
                                  idmtx
                                  [ rotatedCube 0
                                  , rotatedCube $ pi * 2 / 3
                                  , rotatedCube $ pi * 4 / 3 ]

            -- To set the value of an uniform in the program we need to create
            -- a 'Global', which is an uniform name associated with a value. We
            -- can then add it to a single Object or a group of Objects with
            -- the (:~>) operator. Globals are generally created with the -=
            -- operator, but Textures require a special treatment and their
            -- Globals are created with the 'withTexture' function. Note that
            -- the "uniform name" can be any function with the uniform datatype
            -- as return value, so we can use its constructor.
            timeNoiseGroup =    Time -= time / 2
                            :~> withTexture noiseTex (NoiseTexture -=)
                            :~> viewGroup
            in layer prg timeNoiseGroup
        where prg :: Program Uniforms Attributes
              prg = program vertexShader fragmentShader

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

main :: IO ()
main = do t <- loadTexture "../tex.png"
          noise <- loadTexture "../clouds.png"
          animation $ scene t noise
