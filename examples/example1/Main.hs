{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Arrow
import Control.Applicative
import Data.Semigroup
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Blend (transparency, withBlendMode)
import Graphics.Rendering.Ombra.Draw.Mode
import Graphics.Rendering.Ombra.Vector

import Utils.Play (animation)
import Utils.Lighting
import Utils.Shapes (cube)
import Utils.Transform
import Utils.Vertex3D

main :: IO ()
main = animation init render
        where init = snd <$> createBuffers 256 256 gInfo depthInfo (return ())
              gInfo = byteGBufferInfo $ potLinear False
              depthInfo = depthBufferInfo $ parameters Nearest Nearest

render :: BufferPair GVec4 -> Float -> Draw GVec4 ()
render reflectionBuffers time =
        do let mode = commonMode False time
               reflectionMode = commonMode True time
           drawRoute $ sceneWithoutWater mode
           drawBuffers reflectionBuffers $
                   do clearColor >> clearDepth
                      drawRoute $ sceneWithoutWater reflectionMode
           drawRoute $ water (gBuffer reflectionBuffers) time mode
        where sceneWithoutWater mode =    walls mode
                                       <> rotatingCube time 0 mode
                                       <> rotatingCube time 2.09 mode
                                       <> rotatingCube time 4.19 mode

-- | This is the type of root of all the other DrawModes.
type CommonMode = DrawMode GHVertex3D   -- ^ The vertex shader output of the
                                        -- children. It represents an already
                                        -- transformed vertex, and the common
                                        -- draw mode further transforms it by
                                        -- applying the view matrix.

                           GVertex3D    -- ^ The information passed from the
                                        -- vertex shader to the fragment shader.

                           ( GVertex3D
                           , (GVec4, GSpecular)
                           )            -- ^ The fragment shader output of the
                                        -- children. That is, the computed
                                        -- color, the specular parameters and
                                        -- the output of the vertex shader.

                           GVec4        -- ^ The final output of the DrawMode.
                                        -- In this case, it's the output of
                                        -- the fragment shader.

commonMode :: Bool -> Float -> CommonMode
commonMode reflection time =
        if reflection
        then mode (vertexShader mirrorViewMat) fragmentShader
        else mode (vertexShader viewMat) fragmentShader

        where viewMat =     rotYMat4 (sin (time / 2) / 4)
                        .*. transMat4 (Vec3 0 0.5 (- 1.8))
              mirrorMat = reflectionMat4 (Vec4 0 1 0 (- 0.5))
              mirrorViewMat = mirrorMat .*. viewMat
              lightPos = Vec4 (cos time) (1.7 - sin (time / 5) / 3) 0 1
              light = Light { lightPosition = extract $ viewMat .* lightPos
                            , lightAmbient = Vec3 0.4 0.4 0.4
                            , lightDiffuse = Vec3 0.7 0.7 0.7
                            , lightAttenuation = Vec3 0 0.12 0.1
                            }
              vertexShader view = transform ~< view >>> perspective
              fragmentShader = applyLight ~< light


rotatingCube :: Float -> Float -> CommonMode -> DrawRoute GVec4
rotatingCube time off = routeShader cube
                                    (extendGVertex3D >>> transform ~< transMat)
                                    (arr $ id &&& const (color, specular))
        where angle = time + off
              scale = scaleMat4 (Vec3 0.2 0.2 0.2)
              rot = rotYMat4 (time * 3)
              trans = transMat4 $ Vec3 (sin angle) 0.5 (cos angle)
              transMat = trans .*. scale .*. rot
              color = GVec4 0.8 0.2 0 1
              specular = GSpecular 4 1

walls :: CommonMode -> DrawRoute GVec4
walls = routeShader cube
                    (extendGVertex3D >>> transform ~< scaleMat4 (Vec3 2 2 2))
                    (arr id &&& (applyTexture ~< tex &&& arr (const specular)))
        where specular = GSpecular 30 1

water :: GBuffer GVec4 -> Float -> CommonMode -> DrawRoute GVec4
water buffer time =   route cube
                    . blend (const transparency)
                    . preVertex vertexShader 
                    . preFragment fragmentShader
        where transMat =     transMat4 (Vec3 0 (-2) 0)
                         .*. scaleMat4 (Vec3 2.01 1 2.01)
              specular = GSpecular 1 0.5
              vertexShader = extendGVertex3D >>> transform ~< transMat
              fragmentShader =     arr id &&& applyWater ~< (time * 2, buffer)
                               >>^ second (arr $ id &&& const specular)

extendGVertex3D :: Shader s GVertex3D GHVertex3D
extendGVertex3D = sarr $ \(GVertex3D pos norm uv) ->
                        GHVertex3D (pos ^| 1) (norm ^| 0) uv

transform :: VertexShader (GMat4, GHVertex3D) GHVertex3D
transform = sarr $ \(transMat, GHVertex3D pos norm uv) ->
                        GHVertex3D (transMat .* pos) (transMat .* norm) uv

perspective :: VertexShader GHVertex3D (GVec4, GVertex3D)
perspective =    sarr (\(mat, GHVertex3D pos norm uv) ->
                        (mat .* pos, GVertex3D (extract pos) (extract norm) uv))
              ~< perspectiveMat4 0.1 10000 90 1

reverseNormal :: FragmentShader GVertex3D GVertex3D
reverseNormal = farr $ \fragment (GVertex3D pos norm uv) ->
                        let factor = ifB (fragFrontFacing fragment) 1 (-1)
                        in GVertex3D pos (factor *^ norm) uv

applyLight :: FragmentShader (GLight, (GVertex3D, (GVec4, GSpecular))) GVec4
applyLight = shader $     second (first reverseNormal)
                      >>> lightColor &&& arr (fst . snd . snd)
                      >>^ (\(GVec3 lr lg lb, GVec4 r g b a) ->
                              clamp (GVec4 0 0 0 0) (GVec4 1 1 1 1) $
                                GVec4 (lr * r) (lg * g) (lb * b) a)
        where lightColor = second (\(GVertex3D pos norm _, (_, specular)) ->
                                        ((pos, norm), (1, specular)))
                           ^>> pointLight

applyTexture :: FragmentShader (TextureSampler, GVertex3D) GVec4
applyTexture = sarr $ \(sampler, GVertex3D _ _ (GVec2 s t)) ->
                        sampleTexture sampler $ GVec2 s (1 - t)

applyWater :: FragmentShader ((GFloat, GBufferSampler GVec4), GVertex3D) GVec4
applyWater = sarr $ \((time, sampler), GVertex3D _ _ (GVec2 s t)) ->
                        let n = (sin (t * 40 + time) + cos (s * 30 + time)) / 90
                            st' = GVec2 (s + n) (t + n)
                            GVec4 r g b _ = 0.5 *^ sampleGBuffer sampler st'
                        in GVec4 (r + 0.10) (g + 0.15) (b + 0.15) (0.6 + n * 7)

tex :: Texture
tex = mkTexture 128 128 (potLinear True) [cols]
        where cols = [ let (mx, my) = (mod x 16, mod y 16)
                       in if mx == 0 || my == 0 || mx == 15 || my == 15
                          then black
                          else visible (135 + mx * 8) (100 + mx * 8) (mx * 8)
                     | x <- [0 .. 127], y <- [0 .. 127]
                     ]
