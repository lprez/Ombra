{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, ConstraintKinds,
             KindSignatures, DataKinds, GADTs, RankNTypes, FlexibleInstances,
             ScopedTypeVariables, TypeOperators, ImpredicativeTypes,
             TypeSynonymInstances, FlexibleContexts #-}

module Graphics.Rendering.Ombra.Shader.Program (
        MonadProgram(..),
        LoadedProgram(..),
        Compatible,
        Program,
        ProgramIndex,
        program,
        setProgram,
        UniformLocation(..),
        setUniformValue,
        DefaultUniforms2D,
        DefaultAttributes2D,
        DefaultUniforms3D,
        DefaultAttributes3D,
        defaultProgram3D,
        defaultProgram2D,
        programIndex
) where

import Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Graphics.Rendering.Ombra.Shader.Default2D as Default2D
import qualified Graphics.Rendering.Ombra.Shader.Default3D as Default3D
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.GLSL
import Graphics.Rendering.Ombra.Shader.ShaderVar (ShaderVar, ShaderVars)
import Graphics.Rendering.Ombra.Shader.Stages
import Graphics.Rendering.Ombra.Internal.GL hiding (Program, UniformLocation)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Internal.TList
import Unsafe.Coerce

-- | A vertex shader associated with a compatible fragment shader.
data Program (gs :: [*]) (is :: [*]) =
        Program (String, [(String, Int)]) String Int

data LoadedProgram = LoadedProgram !GL.Program (H.HashMap String Int) Int

newtype ProgramIndex = ProgramIndex Int deriving Eq

newtype UniformLocation = UniformLocation GL.UniformLocation

-- | The uniforms used in the default 3D program.
type DefaultUniforms3D = Default3D.Uniforms

-- | The attributes used in the default 3D program.
type DefaultAttributes3D = Default3D.Geometry3D

-- | The uniforms used in the default 2D program.
type DefaultUniforms2D = Default2D.Uniforms

-- | The attributes used in the default 2D program.
type DefaultAttributes2D = Default2D.Geometry2D

instance Hashable (Program gs is) where
        hashWithSalt salt (Program _ _ h) = hashWithSalt salt h

instance Eq (Program gs is) where
        (Program _ _ h) == (Program _ _ h') = h == h'

instance Hashable LoadedProgram where
        hashWithSalt salt (LoadedProgram _ _ h) = hashWithSalt salt h

instance Eq LoadedProgram where
        (LoadedProgram _ _ h) == (LoadedProgram _ _ h') = h == h'

instance GLES => Resource (Program g i) LoadedProgram GL where
        -- TODO: err check!
        loadResource i = loadProgram i
        unloadResource _ (LoadedProgram p _ _) = deleteProgram p

instance GLES => Resource (LoadedProgram, String) UniformLocation GL where
        loadResource (LoadedProgram prg _ _, g) =
                do loc <- getUniformLocation prg $ toGLString g
                   return . Right $ UniformLocation loc
        unloadResource _ _ = return ()

-- | Compatible shaders.
type Compatible pgs vgs fgs =
        EqualOrErr pgs (Union vgs fgs)
                   (Text "Incompatible shader uniforms" :$$:
                    Text "    Vertex shader uniforms: " :<>:
                    ShowType vgs :$$:
                    Text "    Fragment shader uniforms: " :<>:
                    ShowType fgs :$$:
                    Text "    United shader uniforms: " :<>:
                    ShowType (Union vgs fgs) :$$:
                    Text "    Program uniforms: " :<>:
                    ShowType pgs)

-- | Create a 'Program' from the shaders.
program :: ( ShaderVars vgs, ShaderVars vis, VOShaderVars os , ShaderVars fgs
           , Compatible pgs vgs fgs )
        => VertexShader vgs vis os -> FragmentShader fgs os
        -> Program pgs vis
program vs fs = let (vss, attrs) = vertexToGLSLAttr vs
                    fss = fragmentToGLSL fs
                in Program (vss, attrs) fss (hash (vss, fss))

programIndex :: Program gs is -> ProgramIndex
programIndex (Program _ _ h) = ProgramIndex h

defaultProgram3D :: Program DefaultUniforms3D DefaultAttributes3D
defaultProgram3D = program Default3D.vertexShader Default3D.fragmentShader

defaultProgram2D :: Program DefaultUniforms2D DefaultAttributes2D
defaultProgram2D = program Default2D.vertexShader Default2D.fragmentShader

class (GLES, MonadGL m) => MonadProgram m where
        withProgram :: Program gs is -> (LoadedProgram -> m ()) -> m ()
        getUniform :: String -> m (Either String UniformLocation)

setUniformValue :: (MonadProgram m, ShaderVar g, Uniform s g)
                => proxy (s :: CPUSetterType *)
                -> g
                -> CPU s g
                -> m ()
setUniformValue p g c = withUniforms p g c $ \n ug uc ->
        getUniform (uniformName g n) >>= \eu ->
                case eu of
                     Right (UniformLocation l) -> gl $ setUniform l ug uc
                     Left _ -> return ()

setProgram :: MonadProgram m => Program gs is -> m ()
setProgram p = withProgram p $ \(LoadedProgram glp _ _) -> gl $ useProgram glp

loadProgram :: GLES => Program g i -> GL (Either String LoadedProgram)
loadProgram (Program (vss, attrs) fss h) =
        do glp <- createProgram
  
           vs <- loadSource gl_VERTEX_SHADER vss
           fs <- loadSource gl_FRAGMENT_SHADER fss

           vsStatus <- getShaderParameterBool vs gl_COMPILE_STATUS
           fsStatus <- getShaderParameterBool fs gl_COMPILE_STATUS

           if isTrue vsStatus && isTrue fsStatus
           then do attachShader glp vs
                   attachShader glp fs
  
                   locs <- bindAttribs glp 0 attrs []
                   linkProgram glp

                   -- TODO: error check
  
                   -- TODO: ??
                   {-
                   detachShader glp vs
                   detachShader glp fs
                   -}
  
                   return . Right $ LoadedProgram glp
                                                  (H.fromList locs)
                                                  (hash glp)
           else do vsError <- shaderError vs vsStatus "Vertex shader"
                   fsError <- shaderError fs fsStatus "Fragment shader"

                   return . Left $ vsError ++ fsError

        where bindAttribs _ _ [] r = return r
              bindAttribs glp i ((nm, sz) : xs) r =
                        bindAttribLocation glp (fromIntegral i) (toGLString nm)
                        >> bindAttribs glp (i + sz) xs ((nm, i) : r)

              shaderError :: GLES => Shader -> GLBool -> String -> GL String
              shaderError _ b _ | isTrue b = return ""
              shaderError s _ name = getShaderInfoLog s >>= \err ->
                        return $ name ++ " error:" ++ fromGLString err ++ "\n"

loadSource :: GLES => GLEnum -> String -> GL Shader
loadSource ty src =
        do shader <- createShader ty
           shaderSource shader $ toGLString src
           compileShader shader
           return shader
