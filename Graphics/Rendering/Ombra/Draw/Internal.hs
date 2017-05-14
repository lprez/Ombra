{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses, KindSignatures,
             GeneralizedNewtypeDeriving, PolyKinds, TypeOperators #-}

module Graphics.Rendering.Ombra.Draw.Internal (
        Draw,
        DrawState,
        ResStatus(..),
        drawState,
        drawInit,
        clearBuffers,
        drawLayer,
        drawObject,
        preloadGeometry,
        preloadTexture,
        preloadProgram,
        removeGeometry,
        removeTexture,
        removeProgram,
        checkGeometry,
        checkTexture,
        checkProgram,
        textureSize,
        setProgram,
        resizeViewport,
        runDraw,
        execDraw,
        evalDraw,
        gl,
        drawGet
) where

import qualified Graphics.Rendering.Ombra.Blend.Internal as Blend
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Geometry.Internal
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Layer.Internal
import Graphics.Rendering.Ombra.Layer.Types
import Graphics.Rendering.Ombra.Object.Internal
import Graphics.Rendering.Ombra.Object.Types
import Graphics.Rendering.Ombra.Texture.Internal
import Graphics.Rendering.Ombra.Texture.Types
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture, Program, Buffer,
                                                    UniformLocation, cullFace,
                                                    depthMask, colorMask)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Screen
import Graphics.Rendering.Ombra.Shader.Program
import qualified Graphics.Rendering.Ombra.Stencil.Internal as Stencil
import Graphics.Rendering.Ombra.Vector

import Data.Hashable (Hashable)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- | The state of the 'Draw' monad.
data DrawState = DrawState {
        currentProgram :: Maybe ProgramIndex,
        loadedProgram :: Maybe LoadedProgram,
        programs :: ResMap LoadedProgram,
        uniforms :: ResMap UniformLocation,
        elemBuffers :: ResMap LoadedBuffer,
        attributes :: ResMap LoadedAttribute,
        geometries :: ResMap LoadedGeometry,
        textureImages :: ResMap LoadedTexture,
        activeTextures :: Int,
        viewportSize :: (Int, Int),
        blendMode :: Maybe Blend.Mode,
        stencilMode :: Maybe Stencil.Mode,
        cullFace :: Maybe CullFace,
        depthTest :: Bool,
        depthMask :: Bool,
        colorMask :: (Bool, Bool, Bool, Bool)
}

-- | A state monad on top of 'GL'.
newtype Draw a = Draw { unDraw :: StateT DrawState GL a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance EmbedIO Draw where
        embedIO f (Draw a) = Draw get >>= Draw . lift . embedIO f . evalStateT a

instance GLES => MonadScreen Draw where
        currentViewport = viewportSize <$> Draw get
        resizeViewport w h = do setViewport w h
                                Draw . modify $ \s ->
                                        s { viewportSize = (w, h) }

instance GLES => MonadProgram Draw where
        withProgram p act =
                do current <- currentProgram <$> Draw get
                   when (current /= Just (programIndex p)) $
                           getProgram p >>= \elp ->
                                case elp of
                                     Right lp -> do Draw . modify $ \s ->
                                                     s { currentProgram = Just $
                                                             programIndex p
                                                       , loadedProgram = Just lp
                                                       , activeTextures = 0
                                                       }
                                                    act lp
                                     Left _ -> return ()
        getUniform name = do mprg <- loadedProgram <$> Draw get
                             case mprg of
                                  Just prg -> do map <- uniforms <$> Draw get
                                                 gl $ getResource' prg
                                                                   (prg, name)
                                                                   map
                                  Nothing -> return $ Left "No loaded program."

instance GLES => MonadDrawingMode Draw where
        withBlendMode m a = stateReset blendMode setBlendMode m a
        withStencilMode m a = stateReset stencilMode setStencilMode m a
        withDepthTest d a = stateReset depthTest setDepthTest d a
        withDepthMask m a = stateReset depthMask setDepthMask m a
        withColorMask m a = stateReset colorMask setColorMask m a
        withCulling face a = stateReset cullFace setCullFace face a

        -- where tupleToVec (x, y) = Vec2 (fromIntegral x) (fromIntegral y)

instance GLES => MonadTexture Draw where
        getTexture (TextureLoaded l) = return $ Right l
        getTexture (TextureImage t) = getTextureImage t
        getActiveTexturesCount = activeTextures <$> Draw get
        setActiveTexturesCount n = Draw . modify  $ \s ->
                                        s { activeTextures = n }
        newTexture w h fm fM = gl $ LoadedTexture w' h' <$> emptyTexture fm fM
                where (w', h') = (fromIntegral w, fromIntegral h)
        unusedTexture = removeTexture . TextureLoaded

instance GLES => MonadGeometry Draw where
        getAttribute = getDrawResource gl attributes
        getElementBuffer = getDrawResource gl elemBuffers
        getGeometry = getDrawResource id geometries

instance MonadGL Draw where
        gl = Draw . lift

-- | Create a 'DrawState'.
drawState :: GLES
          => Int         -- ^ Viewport width
          -> Int         -- ^ Viewport height
          -> IO DrawState
drawState w h = do programs <- newGLResMap
                   elemBuffers <- newGLResMap
                   attributes <- newGLResMap
                   geometries <- newDrawResMap
                   uniforms <- newGLResMap
                   textureImages <- newGLResMap
                   return DrawState { currentProgram = Nothing
                                    , loadedProgram = Nothing
                                    , programs = programs
                                    , elemBuffers = elemBuffers
                                    , attributes = attributes
                                    , geometries = geometries
                                    , uniforms = uniforms
                                    , textureImages = textureImages
                                    , activeTextures = 0
                                    , viewportSize = (w, h)
                                    , blendMode = Nothing
                                    , depthTest = True
                                    , depthMask = True
                                    , stencilMode = Nothing
                                    , cullFace = Nothing
                                    , colorMask = (True, True, True, True)
                                    }

        where newGLResMap :: IO (ResMap r)
              newGLResMap = newResMap
              
              newDrawResMap :: IO (ResMap r)
              newDrawResMap = newResMap

-- | Initialize the render engine.
drawInit :: GLES => Draw ()
drawInit = viewportSize <$> Draw get >>=
           \(w, h) -> gl $ do clearColor 0.0 0.0 0.0 1.0
                              enable gl_DEPTH_TEST
                              depthFunc gl_LESS
                              viewport 0 0 (fromIntegral w) (fromIntegral h)


{-
maxTexs :: (Integral a, GLES) => a
maxTexs = fromIntegral gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
-}

-- | Run a 'Draw' action.
runDraw :: Draw a
        -> DrawState
        -> GL (a, DrawState)
runDraw (Draw a) = runStateT a

-- | Execute a 'Draw' action.
execDraw :: Draw a              -- ^ Action.
         -> DrawState           -- ^ State.
         -> GL DrawState
execDraw (Draw a) = execStateT a

-- | Evaluate a 'Draw' action.
evalDraw :: Draw a              -- ^ Action.
         -> DrawState           -- ^ State.
         -> GL a
evalDraw (Draw a) = evalStateT a

left :: Either String a -> Maybe String
left (Left x) = Just x
left _ = Nothing

-- | Manually allocate a 'Geometry' in the GPU. Eventually returns an error
-- string.
preloadGeometry :: GLES => Geometry (i ': is) -> Draw (Maybe String)
preloadGeometry g = left <$> getGeometry g

-- | Manually allocate a 'Texture' in the GPU.
preloadTexture :: GLES => Texture -> Draw (Maybe String)
preloadTexture t = left <$> getTexture t

-- | Manually allocate a 'Program' in the GPU.
preloadProgram :: GLES => Program gs is -> Draw (Maybe String)
preloadProgram p = left <$> getProgram p

-- | Manually delete a 'Geometry' from the GPU.
removeGeometry :: GLES => Geometry (i ': is) -> Draw ()
removeGeometry g = removeDrawResource id geometries g

-- | Manually delete a 'Texture' from the GPU.
removeTexture :: GLES => Texture -> Draw ()
removeTexture (TextureImage i) = removeDrawResource gl textureImages i
removeTexture (TextureLoaded l) = gl $ unloadResource
                                        (Nothing :: Maybe TextureImage) l

-- | Manually delete a 'Program' from the GPU.
removeProgram :: GLES => Program gs is -> Draw ()
removeProgram = removeDrawResource gl programs

-- | Check if a 'Geometry' failed to load.
checkGeometry :: GLES => Geometry (i ': is) -> Draw (ResStatus ())
checkGeometry g = fmap (const ()) <$> checkDrawResource id geometries g

-- | Check if a 'Texture' failed to load. Eventually returns the texture width
-- and height.
checkTexture :: (GLES, Num a) => Texture -> Draw (ResStatus (a, a))
checkTexture (TextureImage i) =
        fmap loadedTextureSize <$> checkDrawResource gl textureImages i
checkTexture (TextureLoaded l) = return $ Loaded (loadedTextureSize l)

loadedTextureSize :: (GLES, Num a) => LoadedTexture -> (a, a)
loadedTextureSize (LoadedTexture w h _) = (fromIntegral w, fromIntegral h)

-- | Check if a 'Program' failed to load.
checkProgram :: GLES => Program gs is -> Draw (ResStatus ())
checkProgram p = fmap (const ()) <$> checkDrawResource gl programs p

stateReset :: (DrawState -> a) -> (a -> Draw ()) -> a -> Draw b -> Draw b
stateReset getOld set new act = do old <- getOld <$> Draw get
                                   set new
                                   b <- act
                                   set old
                                   return b

getTextureImage :: GLES => TextureImage
                -> Draw (Either String LoadedTexture)
getTextureImage = getDrawResource gl textureImages

getProgram :: GLES => Program gs is -> Draw (Either String LoadedProgram)
getProgram = getDrawResource gl programs

setBlendMode :: GLES => Maybe Blend.Mode -> Draw ()
setBlendMode Nothing = do m <- blendMode <$> Draw get
                          case m of
                               Just _ -> gl $ disable gl_BLEND
                               Nothing -> return ()
                          Draw . modify $ \s -> s { blendMode = Nothing }
setBlendMode (Just newMode) =
        do mOldMode <- blendMode <$> Draw get
           case mOldMode of
                Nothing -> do gl $ enable gl_BLEND
                              changeColor >> changeEquation >> changeFunction
                Just oldMode ->
                     do when (Blend.constantColor oldMode /= constantColor)
                                changeColor
                        when (Blend.equation oldMode /= equation)
                                changeEquation
                        when (Blend.function oldMode /= function)
                                changeFunction
           Draw . modify $ \s -> s { blendMode = Just newMode }
        where constantColor = Blend.constantColor newMode
              equation@(rgbEq, alphaEq) = Blend.equation newMode
              function@(rgbs, rgbd, alphas, alphad) = Blend.function newMode
              changeColor = case constantColor of
                                 Just (Vec4 r g b a) -> gl $ blendColor r g b a
                                 Nothing -> return ()
              changeEquation = gl $ blendEquationSeparate rgbEq alphaEq
              changeFunction = gl $ blendFuncSeparate rgbs rgbd
                                                      alphas alphad

setStencilMode :: GLES => Maybe Stencil.Mode -> Draw ()
setStencilMode Nothing = do m <- stencilMode <$> Draw get
                            case m of
                                 Just _ -> gl $ disable gl_STENCIL_TEST
                                 Nothing -> return ()
                            Draw . modify $ \s -> s { stencilMode = Nothing }
setStencilMode (Just newMode@(Stencil.Mode newFun newOp)) =
        do mOldMode <- stencilMode <$> Draw get
           case mOldMode of
                Nothing -> do gl $ enable gl_STENCIL_TEST
                              sides newFun changeFunction
                              sides newOp changeOperation
                Just (Stencil.Mode oldFun oldOp) ->
                        do when (oldFun /= newFun) $
                                sides newFun changeFunction
                           when (oldOp /= newOp) $
                                sides newOp changeOperation
           Draw . modify $ \s -> s { stencilMode = Just newMode }
        where changeFunction face f = let (t, v, m) = Stencil.function f
                                      in gl $ stencilFuncSeparate face t v m
              changeOperation face o = let (s, d, n) = Stencil.operation o
                                       in gl $ stencilOpSeparate face s d n
              sides (Stencil.FrontBack x) f = f gl_FRONT_AND_BACK x
              sides (Stencil.Separate x y) f = f gl_FRONT x >> f gl_BACK y

setCullFace :: GLES => Maybe CullFace -> Draw ()
setCullFace Nothing = do old <- cullFace <$> Draw get
                         case old of
                              Just _ -> gl $ disable gl_CULL_FACE
                              Nothing -> return ()
                         Draw . modify $ \s -> s { cullFace = Nothing }
setCullFace (Just newFace) =
        do old <- cullFace <$> Draw get
           when (old == Nothing) . gl $ enable gl_CULL_FACE
           case old of
                Just oldFace | oldFace == newFace -> return ()
                _ -> gl . GL.cullFace $ case newFace of
                                             CullFront -> gl_FRONT
                                             CullBack -> gl_BACK
                                             CullFrontBack -> gl_FRONT_AND_BACK
           Draw . modify $ \s -> s { cullFace = Just newFace }
                   
setDepthTest :: GLES => Bool -> Draw ()
setDepthTest = setFlag depthTest (\x s -> s { depthTest = x })
                       (gl $ enable gl_DEPTH_TEST) (gl $ disable gl_DEPTH_TEST)
                   
setDepthMask :: GLES => Bool -> Draw ()
setDepthMask = setFlag depthMask (\x s -> s { depthMask = x })
                       (gl $ GL.depthMask true) (gl $ GL.depthMask false)

setFlag :: (DrawState -> Bool)
        -> (Bool -> DrawState -> DrawState)
        -> Draw ()
        -> Draw ()
        -> Bool
        -> Draw ()
setFlag getF setF enable disable new =
        do old <- getF <$> Draw get
           case (old, new) of
                   (False, True) -> enable
                   (True, False) -> disable
                   _ -> return ()
           Draw . modify $ setF new

setColorMask :: GLES => (Bool, Bool, Bool, Bool) -> Draw ()
setColorMask new@(r, g, b, a) = do old <- colorMask <$> Draw get
                                   when (old /= new) . gl $
                                           GL.colorMask r' g' b' a'
                                   Draw . modify $ \s -> s { colorMask = new }
        where (r', g', b', a') = (bool r, bool g, bool b, bool a)
              bool True = true
              bool False = false

getDrawResource :: Resource i r m
                => (m (Either String r) -> Draw (Either String r))
                -> (DrawState -> ResMap r)
                -> i
                -> Draw (Either String r)
getDrawResource lft mg i = do
        map <- mg <$> Draw get
        lft $ getResource i map

checkDrawResource :: Resource i r m
                  => (m (ResStatus r) -> Draw (ResStatus r))
                  -> (DrawState -> ResMap r)
                  -> i
                  -> Draw (ResStatus r)
checkDrawResource lft mg i = do
        map <- mg <$> Draw get
        lft $ checkResource i map

removeDrawResource :: (Resource i r m, Hashable i)
                   => (m () -> Draw ())
                   -> (DrawState -> ResMap r)
                   -> i
                   -> Draw ()
removeDrawResource lft mg i = do
        s <- mg <$> Draw get
        lft $ removeResource i s

-- | Get the 'DrawState'.
drawGet :: Draw DrawState
drawGet = Draw get
