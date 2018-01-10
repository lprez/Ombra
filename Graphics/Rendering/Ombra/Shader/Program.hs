{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, ConstraintKinds,
             KindSignatures, DataKinds, GADTs, RankNTypes, FlexibleInstances,
             ScopedTypeVariables, TypeOperators, ImpredicativeTypes,
             TypeSynonymInstances, FlexibleContexts, DefaultSignatures #-}

module Graphics.Rendering.Ombra.Shader.Program (
        LoadedProgram(..),
        Program,
        ProgramIndex,
        program,
        UniformLocation(..),
        setUniform
) where

import Data.Hashable
import qualified Data.HashMap.Strict as H
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.GLSL
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Internal.GL hiding (Program, UniformLocation)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Internal.TList
import Unsafe.Coerce

data Program i o = Program (String, [(String, Int)]) String Int

data LoadedProgram = LoadedProgram !GL.Program (H.HashMap String Int) Int

newtype ProgramIndex = ProgramIndex Int deriving Eq

newtype UniformLocation = UniformLocation GL.UniformLocation

instance Hashable (Program i o) where
        hashWithSalt salt (Program _ _ h) = hashWithSalt salt h

instance Eq (Program i o) where
        (Program _ _ h) == (Program _ _ h') = h == h'

instance Hashable LoadedProgram where
        hashWithSalt salt (LoadedProgram _ _ h) = hashWithSalt salt h

instance Eq LoadedProgram where
        (LoadedProgram _ _ h) == (LoadedProgram _ _ h') = h == h'

instance (GLES, MonadGL m) => MonadLoad (Program g i) LoadedProgram m where
        -- TODO: err check!
        loadResource i = gl $ loadProgram i
        unloadResource _ (LoadedProgram p _ _) = gl $ deleteProgram p

instance (GLES, MonadGL m) => MonadLoad (LoadedProgram, UniformID)
                                        UniformLocation m where
        loadResource (LoadedProgram prg _ _, g) = gl $
                do loc <- getUniformLocation prg . toGLString $ uniformName g
                   return . Right $ UniformLocation loc
        unloadResource _ _ = return ()

-- | Create a 'Program' from the shaders.
program :: (ShaderInput i, ShaderInput v, FragmentShaderOutput o)
        => VertexShader i (GVec4, v)
        -> FragmentShader v o
        -> Program i o
program vs fs = let (vss, (uid, attrs)) = compileVertexShader vs
                    fss = compileFragmentShader uid fs
                in Program (vss, attrs) fss (hash (vs, fs))

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

              shaderError :: GLES => GL.Shader -> GLBool -> String -> GL String
              shaderError _ b _ | isTrue b = return ""
              shaderError s _ name = getShaderInfoLog s >>= \err ->
                        return $ name ++ " error:" ++ fromGLString err ++ "\n"

loadSource :: GLES => GLEnum -> String -> GL GL.Shader
loadSource ty src =
        do shader <- createShader ty
           shaderSource shader $ toGLString src
           compileShader shader
           return shader

setUniform :: ( MonadGL m
              , MonadResource (LoadedProgram, UniformID) UniformLocation m
              , BaseUniform g
              )
           => LoadedProgram
           -> UniformID
           -> proxy g
           -> CPUBase g
           -> m ()
setUniform prg uid g val =
        getResource' (Just prg) (prg, uid) >>= \eu ->
                case eu of
                     Right (UniformLocation l) -> gl $ setBaseUniform l g val
                     Left _ -> return ()
