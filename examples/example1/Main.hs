{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Arrow
import Control.Applicative
import Data.Monoid (mconcat)
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Blend (transparency, withBlendMode)
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
        do draw $ scene viewMat
           withDepthTest False $
                drawBuffers reflectionBuffers
                            (draw $ scene mirrorViewMat)
           withBlendMode (Just transparency) $
                   draw $ water (gBuffer reflectionBuffers) viewMat light time
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
              scene viewMat = mconcat [ walls viewMat light
                                      , rotatingCube viewMat light time 0
                                      , rotatingCube viewMat light time 2.09
                                      , rotatingCube viewMat light time 4.19
                                      ]

rotatingCube :: Mat4 -> Light -> Float -> Float -> Image GVec4
rotatingCube view light time off =
        image (transform ~< (view .*. transformMat) >>> first perspective)
              (const color &&& id ^>> applyLight ~< (light, specular))
              cube
        where transformMat = let angle = time + off
                                 scale = scaleMat4 (Vec3 0.2 0.2 0.2)
                                 rot = rotYMat4 (time * 3)
                                 trans = transMat4 $ Vec3 (sin angle)
                                                          0.5
                                                          (cos angle)
                             in trans .*. scale .*. rot
              color = GVec4 0.8 0.2 0 1
              specular = Specular 4 1

walls :: Mat4 -> Light -> Image GVec4
walls viewMat light = image (    transform ~< (viewMat .*. transformMat)
                             >>> first perspective)
                            (    applyTexture ~< tex &&& arr id
                             >>> applyLight ~< (light, Specular 20 1))
                            cube
        where transformMat = scaleMat4 $ Vec3 2 2 2

water :: GBuffer GVec4 -> Mat4 -> Light -> Float -> Image GVec4
water buffer viewMat light time =
        image (transform ~< (viewMat .*. transformMat) >>> first perspective)
              (    applyWater ~< (time * 2, buffer) &&& arr id
               >>> applyLight ~< (light, specular))
              cube
        where transformMat =     transMat4 (Vec3 0 (-2) 0)
                             .*. scaleMat4 (Vec3 2.01 1 2.01)
              specular = Specular 1 0.5

transform :: VertexShader (GMat4, GVertex3D) (GVec4, GVertex3D)
transform = sarr $ \(viewMat, GVertex3D pos norm uv) ->
                        let pos' = viewMat .* (pos ^| 1)
                            norm' = extract $ viewMat .* (norm ^| 0)
                        in (pos', GVertex3D (extract pos') norm' uv)

perspective :: VertexShader GVec4 GVec4
perspective = sarr (uncurry (.*)) ~< perspectiveMat4 0.1 10000 90 1

reverseNormal :: FragmentShader GVertex3D GVertex3D
reverseNormal = farr $ \fragment (GVertex3D pos norm uv) ->
                        let factor = ifB (fragFrontFacing fragment) 1 (-1)
                        in GVertex3D pos (factor *^ norm) uv

applyLight :: FragmentShader ((GLight, GSpecular), (GVec4, GVertex3D)) GVec4
applyLight = shader $     second (second reverseNormal)
                      >>> (lightInp >>> pointLight) &&& arr (fst . snd)
                      >>^ (\(GVec3 lr lg lb, GVec4 r g b a) ->
                              clamp (GVec4 0 0 0 0) (GVec4 1 1 1 1) $
                                GVec4 (lr * r) (lg * g) (lb * b) a)
        where lightInp = arr $ \((gLight, gSpec), (_, GVertex3D pos norm _)) ->
                                        (gLight, ((pos, norm), (1, gSpec)))

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
