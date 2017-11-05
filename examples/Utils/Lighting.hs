{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Utils.Lighting where

import GHC.Generics
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Vector

data Light = Light { lightPosition :: Vec3
                   , lightAmbient :: Vec3
                   , lightDiffuse :: Vec3
                   , lightAttenuation :: Vec3
                   }
        deriving Generic

data GLight = GLight GVec3 GVec3 GVec3 GVec3 deriving Generic

data Specular = Specular { specularPower :: Float
                         , specularIntensity :: Float
                         }
        deriving Generic

data GSpecular = GSpecular GFloat GFloat deriving Generic

instance MultiShaderType GLight
instance ShaderInput GLight
instance GLES => Uniform GLight where
        type CPUUniform GLight = Light

instance MultiShaderType GSpecular
instance ShaderInput GSpecular
instance FragmentShaderOutput GSpecular
instance GLES => Uniform GSpecular where
        type CPUUniform GSpecular = Specular

pointLight :: FragmentShader (GLight, ((GVec3, GVec3), (GFloat, GSpecular)))
                             GVec3
pointLight = sarr $
        \( (GLight lightPos ambCol difCol attenParams)
         , ( (position, normal)
           , (obscurance, GSpecular specularPow specularInt)
           )
         ) -> let lightToObject = position ^-^ lightPos
                  lightDir = normalized lightToObject
                  lightDist = magnitude lightToObject
                  atten = attenuation attenParams lightDist

                  ambient = obscurance *^ ambCol
                  diffuse = maxG (normal <.> (negateV lightDir)) (0 :: GFloat)

                  eye = normalized $ negateV position
                  ref = normalized $ reflect lightDir normal
                  specular0 = maxG (eye <.> ref) (0.0 :: GFloat)
                  specular = specularInt * specular0 ** specularPow
              in (ambient ^+^ (diffuse ^+^ specular) *^ difCol) ^/ atten

attenuation :: GVec3 -> GFloat -> GFloat
attenuation (GVec3 const linear exp) dist =
        const + linear * dist + exp * dist * dist
