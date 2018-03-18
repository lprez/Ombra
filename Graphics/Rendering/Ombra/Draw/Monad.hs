{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses, KindSignatures,
             GeneralizedNewtypeDeriving, StandaloneDeriving, TypeOperators, CPP,
             ScopedTypeVariables, UndecidableInstances, TypeFamilies #-}

module Graphics.Rendering.Ombra.Draw.Monad (
        DrawT,
        Draw,
        Buffer(..),
        runDrawT,
        {-
        preloadGeometry,
        preloadTexture,
        preloadProgram,
        -- TODO:
        removeGeometry,
        removeTexture,
        removeProgram,
        checkGeometry,
        checkTexture,
        checkProgram,
        setProgram,
        -}
) where

import qualified Data.List.NonEmpty as L
import Data.Hashable
import qualified Data.HashMap.Lazy as H
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.Word
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Fail
import Control.Monad.Zip
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State hiding (state)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class

import qualified Graphics.Rendering.Ombra.Blend.Draw as Blend
import qualified Graphics.Rendering.Ombra.Blend.Types as Blend
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Culling
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Draw.State
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.OutBuffer (gBuffer, depthBuffer)
import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Texture
import Graphics.Rendering.Ombra.Texture.Draw
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture, Program, Buffer,
                                                    UniformLocation, cullFace,
                                                    depthMask, colorMask,
                                                    drawBuffers, clearColor,
                                                    clearDepth, clearStencil)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Screen
import Graphics.Rendering.Ombra.Shader (TextureSampler)
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.Types
import qualified Graphics.Rendering.Ombra.Stencil.Draw as Stencil
import qualified Graphics.Rendering.Ombra.Stencil.Types as Stencil
import Graphics.Rendering.Ombra.Vector

data ExtDrawState o = ExtDrawState { ctx :: Ctx
                                   , activeTextures :: H.HashMap Texture Int
                                   , activeTexturesHoles :: S.Set Int
                                   , loadedProgram :: Maybe LoadedProgram
                                   , defaultFrameBuffer :: FrameBuffer
                                   , targetFrameBuffer :: Maybe FrameBuffer
                                   , defaultViewport :: ((Int, Int), (Int, Int))
                                   , targetViewport :: ((Int, Int), (Int, Int))
                                   , drawState :: DrawState o
                                   }

type Draw o = DrawT o IO

newtype DrawT o m a = DrawT
        { unDrawT :: StateT (ExtDrawState o)
                            (Resource3T LoadedTexture
                                        LoadedProgram
                                        LoadedGeometry
                                        (Resource3T LoadedBuffer
                                                    LoadedAttribute
                                                    UniformLocation
                                                    m))
                            a
        }
        deriving ( Functor
                 , Applicative
                 , Alternative
                 , Monad
                 , MonadFix
                 , MonadFail
                 , MonadIO
                 , MonadPlus
                 )

deriving instance MonadCont m => MonadCont (DrawT o m)
deriving instance MonadRWS r w s m => MonadRWS r w s (DrawT o m)
deriving instance MonadReader r m => MonadReader r (DrawT o m)
deriving instance (MonadWriter w m, Monoid w) => MonadWriter w (DrawT o m)
deriving instance MonadError e m => MonadError e (DrawT o m)

instance MonadState s m => MonadState s (DrawT o m) where
        get = DrawT . lift . lift . lift $ get
        put = DrawT . lift . lift . lift . put
        state = DrawT . lift . lift . lift . state

instance MonadBase b m => MonadBase b (DrawT o m) where
        liftBase = liftBaseDefault

instance MonadTrans (DrawT o) where
        lift = DrawT . lift . lift . lift

instance MonadTransControl (DrawT o) where
        type StT (DrawT o) a =
                StT (Resource3T LoadedTexture LoadedProgram LoadedGeometry)
                    (StT (Resource3T LoadedBuffer LoadedAttribute
                                     UniformLocation)
                         (StT (StateT (ExtDrawState o)) a))
        liftWith f = DrawT $ liftWith $ \run0 -> liftWith $ \run1 ->
                        liftWith $ \run2 -> f $ run2 . run1 . run0 . unDrawT
        restoreT = DrawT . restoreT . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (DrawT o m) where
        type StM (DrawT o m) a = ComposeSt (DrawT o) m a
        liftBaseWith = defaultLiftBaseWith
        restoreM = defaultRestoreM

instance MonadBase IO m => MonadGL (DrawT o m) where
        gl a = ctx <$> DrawT get >>= liftBase . runReaderT (unGL a)

instance (FragmentShaderOutput o, MonadBaseControl IO m, GLES) =>
        MonadDraw o (DrawT o m) where
        type BufferDraw o' (DrawT o m) = DrawT o' m
        currentState = drawState <$> DrawT get
        switchState = switchStateDiff
        {-
        withColorMask m a = stateReset colorMask setColorMask m a
        withDepthTest d a = stateReset depthTest setDepthTest d a
        withDepthMask m a = stateReset depthMask setDepthMask m a
        -}
        clearColor = clearBuffers [ColorBuffer]
        clearColorWith (Vec4 r g b a) = gl $ do GL.clearColor (realToFrac r)
                                                              (realToFrac g)
                                                              (realToFrac b)
                                                              (realToFrac a)
                                                clearBuffers [ColorBuffer]
                                                GL.clearColor 0.0 0.0 0.0 1.0
        clearDepth = clearBuffers [DepthBuffer]
        clearDepthWith value = gl $ do GL.clearDepth $ realToFrac value
                                       clearBuffers [DepthBuffer]
                                       GL.clearDepth 1
        clearStencil = clearBuffers [StencilBuffer]
        clearStencilWith value = gl $ do GL.clearStencil $ fromIntegral value
                                         clearBuffers [StencilBuffer]
                                         GL.clearStencil 0
        createBuffers w h gBufferInfo depthBufferInfo draw =
                do (ret, gBuffer, depthBuffer) <-
                        drawBuffers' w h
                                     True
                                     (Right gBufferInfo)
                                     (Right depthBufferInfo)
                                     draw
                   return (ret, BufferPair gBuffer depthBuffer)
        createGBuffer gBufferInfo depthBuffer draw =
                do let (w, h) = bufferSize depthBuffer
                   (ret, gBuffer, _) <-
                        drawBuffers' w h
                                     True
                                     (Right gBufferInfo)
                                     (Left depthBuffer)
                                     draw
                   return (ret, BufferPair gBuffer depthBuffer)
        createDepthBuffer gBuffer depthBufferInfo draw =
                do let (w, h) = bufferSize gBuffer
                   (ret, _, depthBuffer) <-
                        drawBuffers' w h
                                     True
                                     (Left gBuffer)
                                     (Right depthBufferInfo)
                                     draw
                   return (ret, BufferPair gBuffer depthBuffer)
        drawBuffers (BufferPair gBuffer depthBuffer) draw =
                do let (w, h) = bufferSize gBuffer
                   (ret, _, _) <- drawBuffers' w h
                                               True
                                               (Left gBuffer)
                                               (Left depthBuffer)
                                               draw
                   return ret

instance (GLES, MonadBaseControl IO m) => MonadRead GVec4 (DrawT GVec4 m) where
        readColor = flip readPixels gl_RGBA
        readColorFloat = flip readPixels gl_RGBA
        readDepth = flip readPixels gl_DEPTH_COMPONENT
        readDepthFloat = flip readPixels gl_DEPTH_COMPONENT
        readStencil = flip readPixels gl_STENCIL_INDEX

instance (GLES, MonadBase IO m) => MonadScreen (DrawT o m) where
        currentViewport = targetViewport <$> DrawT get
        resizeViewport p w = do setViewport p w
                                s <- DrawT get
                                let s' = s { targetViewport = (p, w) }
                                if isNothing (targetFrameBuffer s)
                                then DrawT . put $
                                        s' { defaultViewport = (p, w) }
                                else DrawT . put $ s'
                                        

instance (GLES, MonadBaseControl IO m, GeometryVertex g, ElementType e) =>
        MonadResource (Geometry e g) LoadedGeometry (DrawT o m) where
        getResource' k i = inGL . DrawT . lift $ getResource' k i

instance (GLES, MonadBaseControl IO m) =>
        MonadResource TextureImage LoadedTexture (DrawT o m) where
        getResource' k i = inGL . DrawT . lift $ getResource' k i

instance (GLES, MonadBaseControl IO m) =>
        MonadResource (Program g i) LoadedProgram (DrawT o m) where
        getResource' k i = inGL . DrawT . lift $ getResource' k i

instance (GLES, MonadBaseControl IO m, ElementType e) =>
        MonadResource (Elements e is) LoadedBuffer (DrawT o m) where
        getResource' k i = inGL . DrawT . lift . lift $ getResource' k i

instance (GLES, MonadBaseControl IO m) =>
        MonadResource AttributeData LoadedAttribute (DrawT o m) where
        getResource' k i = inGL . DrawT . lift . lift $ getResource' k i

instance (GLES, MonadBaseControl IO m) =>
        MonadResource (LoadedProgram, UniformID)
                      UniformLocation (DrawT o m) where
        getResource' k i = inGL . DrawT . lift . lift $ getResource' k i

inGL :: MonadBase IO m => DrawT o GL a -> DrawT o m a
inGL a = do c <- ctx <$> DrawT get
            mapDrawT (liftBase . flip runReaderT c . unGL) a

mapDrawT :: (m (a, ExtDrawState o) -> n (b, ExtDrawState o))
         -> DrawT o m a
         -> DrawT o n b
mapDrawT f = DrawT . mapStateT (mapResource3T (mapResource3T f)) . unDrawT

runDrawT :: (GLES, MonadBaseControl IO m)
         => DrawT GVec4 m a
         -> Ctx
         -> Int
         -> Int
         -> m a
runDrawT act ctx w h = runNewResource3T . runNewResource3T $ evalStateT draw ext
        where DrawT draw = initialize (w, h) >> act
              ext = initExtState ctx (w, h)

initExtState :: GLES => Ctx -> (Int, Int) -> ExtDrawState GVec4
initExtState ctx vp = ExtDrawState { ctx = ctx
                                   , activeTextures = H.empty
                                   , activeTexturesHoles = S.fromList [0 .. 256]
                                   -- TODO: glGetIntegerv GL_MAX_TEXTURE_
                                   --                     IMAGE_UNITS
                                   , loadedProgram = Nothing
                                   , targetFrameBuffer = Nothing
                                   , defaultFrameBuffer = noFramebuffer
                                   , defaultViewport = ((0, 0), vp)
                                   , targetViewport = ((0, 0), vp)
                                   , drawState = initialState
                                   }

initialize :: (GLES, MonadBaseControl IO m) => (Int, Int) -> DrawT GVec4 m ()
initialize vp = do gl $ do GL.clearColor 0.0 0.0 0.0 1.0
                           GL.clearDepth 1
                           GL.clearStencil 0
                           depthFunc gl_LESS
                   resizeViewport (0, 0) vp
                   switchStateDiff False initialState . L.toList $
                           rootChanges (initialState :: DrawState GVec4)

switchStateDiff :: (GLES, MonadBaseControl IO m, FragmentShaderOutput o)
                => Bool
                -> DrawState o
                -> [DrawChange]
                -> DrawT o m ()
switchStateDiff draw state@(DrawState { stateGeometry = mGeom }) chgs =
        do flip mapM (reverse chgs) $ \chg -> setFun chg state chg
           case (draw, mGeom) of
                (True, Just geom) -> drawGeometry False geom
                _ -> return ()
           DrawT . modify $ \s -> s { drawState = state }
        where setFun (ChangeTarget _) = setBuffer
              setFun (ClearBuffers _) = const setClear
              setFun (ChangeShaders _) = setProgram
              setFun (ChangeTextures _ _) = const setTextures
              setFun (ChangeBlendEnabled _) = const setBlendMode
              setFun (ChangeBlendConstantColor _) = const setBlendMode
              setFun (ChangeBlendEquation _) = const setBlendMode
              setFun (ChangeBlendFunction _) = const setBlendMode
              setFun (ChangeStencilEnabled _) = const setStencilMode
              setFun (ChangeStencilFunction _) = const setStencilMode
              setFun (ChangeStencilOperation _) = const setStencilMode
              setFun (ChangeCullEnabled _) = const setCulling
              setFun (ChangeCullFace _) = const setCulling
              setFun (ChangeDepthTest _) = const setDepthTest
              setFun (ChangeDepthMask _) = const setDepthMask
              setFun (ChangeColorMask _) = const setColorMask
              setFun (ChangeGeometry _) = setGeometry
              setFun (ChangeUniforms _) = const setUniforms

-- TODO: spostare la parte centrale su Shader.Program
setProgram :: (GLES, MonadBaseControl IO m, FragmentShaderOutput o)
           => DrawState o
           -> DrawChange
           -> DrawT o m ()
setProgram (DrawState { stateVertexShader = Just vs
                      , stateFragmentShader = Just fs})
           (ChangeShaders _) =
        getResource' Nothing (program vs fs) >>= \elp ->
                case elp of
                     Right lp@(LoadedProgram glp _ _) ->
                             do DrawT . modify $ \s ->
                                 s { loadedProgram = Just lp
                                   -- XXX , activeTextures = 0
                                   }
                                gl $ useProgram glp
                     Left err -> error err
setProgram _ (ChangeShaders _) =
        DrawT . modify $ \s -> s { loadedProgram = Nothing }
setProgram _ _ = return ()

setGeometry :: (GLES, MonadBaseControl IO m)
            => DrawState o
            -> DrawChange
            -> DrawT o m ()
setGeometry (DrawState { stateGeometry = geometry })
            (ChangeGeometry _) = setVAO geometry
setGeometry _ _ = return ()

setBuffer :: (GLES, MonadBaseControl IO m)
          => DrawState o
          -> DrawChange
          -> DrawT o m ()
setBuffer (DrawState { stateTarget = BufferTarget bp }) (ChangeTarget _) =
        do deleteCurrentFrameBuffer
           oldFb <- targetFrameBuffer <$> DrawT get
           let (w, h) = bufferSize $ gBuffer bp
           drawUsedBuffers w h (gBuffer bp) (depthBuffer bp) Nothing
           return ()
setBuffer (DrawState { stateTarget = DefaultTarget }) (ChangeTarget _) =
        do deleteCurrentFrameBuffer
           resetFrameBuffer
setBuffer _ _ = return ()

setClear :: (GLES, MonadBaseControl IO m, FragmentShaderOutput o)
         => DrawChange
         -> DrawT o m ()
setClear (ClearBuffers (color, depth, stencil)) =
        do when color $ clearColor
           when depth $ clearDepth
           when stencil $ clearStencil
setClear _ = return ()

setUniforms :: (GLES, MonadBaseControl IO m) => DrawChange -> DrawT o m ()
setUniforms (ChangeUniforms changedUniforms) =
        do texs <- activeTextures <$> DrawT get
           mprg <- loadedProgram <$> DrawT get
           case mprg of
                Just prg -> do H.traverseWithKey (setUniform' prg texs)
                                                 changedUniforms
                               return ()
                Nothing -> return ()
        where setUniform' prg _ uid (UniformValue proxy value) =
                      setUniform prg uid proxy value
              setUniform' prg texs uid (UniformTexture tex) =
                      setUniform prg uid
                                 (Proxy :: Proxy TextureSampler)
                                 (Sampler2D . fromIntegral $ texs H.! tex) 
setUniforms _ = return ()

setTextures :: (GLES, MonadBaseControl IO m) => DrawChange -> DrawT o m ()
setTextures (ChangeTextures added removed) = 
        do oldActives <- activeTextures <$> DrawT get
           oldHoles <- activeTexturesHoles <$> DrawT get
           let removedActives = HS.foldr (\t -> S.insert (oldActives H.! t))
                                         S.empty
                                         removed
               actives = H.difference oldActives (HS.toMap removed)
               holes = S.foldr S.insert oldHoles removedActives
               accHelper tex (hs, map) = let (holeSing, hs') = S.splitAt 1 hs
                                             hole = S.elemAt 0 holeSing
                                         in (hs', H.insert tex hole map)
                                         -- TODO: check holeSing size = 0
               (holes', addedActives) = foldr accHelper
                                              (holes, H.empty)
                                              (HS.toList added)
                                        -- TODO: mapAccumL
               actives' = H.union actives addedActives
           flip mapM_ (S.intersection removedActives holes') $ \i ->
                   gl $ do activeTexture $ gl_TEXTURE0 + fromIntegral i
                           bindTexture gl_TEXTURE_2D noTexture
           flip H.traverseWithKey addedActives $ \tex i ->
                   do etex <- case tex of
                                   TextureImage img -> getResource img
                                   TextureLoaded lt -> return $ Right lt
                      case etex of
                           Right (LoadedTexture _ _ _ t) -> gl $
                                   do let i' = fromIntegral i
                                      activeTexture $ gl_TEXTURE0 + i'
                                      bindTexture gl_TEXTURE_2D t
                           Left _ -> return ()
           DrawT . modify $ \s -> s { activeTextures = actives'
                                    , activeTexturesHoles = holes'
                                    }
setTextures _ = return ()

setBlendMode :: (GLES, MonadBase IO m) => DrawChange -> DrawT o m ()
setBlendMode (ChangeBlendEnabled False) = gl $ disable gl_BLEND
setBlendMode (ChangeBlendEnabled True) = gl $ enable gl_BLEND
setBlendMode (ChangeBlendConstantColor (Just (Vec4 r g b a))) =
        gl $ blendColor r g b a
setBlendMode (ChangeBlendEquation (rgbEq, alphaEq)) =
        gl $ blendEquationSeparate rgbEq alphaEq
setBlendMode (ChangeBlendFunction (rgbs, rgbd, alphas, alphad)) =
        gl $ blendFuncSeparate rgbs rgbd alphas alphad
setBlendMode _ = return ()

setStencilMode :: (GLES, MonadBase IO m) => DrawChange -> DrawT o m ()
setStencilMode chg =
        case chg of
             ChangeStencilEnabled False -> gl $ disable gl_STENCIL_TEST
             ChangeStencilEnabled True -> gl $ enable gl_STENCIL_TEST
             ChangeStencilFunction funs ->
                     sides funs $
                        \face (t, v, m) -> gl $ stencilFuncSeparate face t v m
             ChangeStencilOperation ops ->
                     sides ops $
                        \face (s, d, n) -> gl $ stencilOpSeparate face s d n
             _ -> return ()
        where sides (Stencil.FrontBack x) f = f gl_FRONT_AND_BACK x
              sides (Stencil.Separate x y) f = f gl_FRONT x >> f gl_BACK y

setCulling :: (GLES, MonadBase IO m) => DrawChange -> DrawT o m ()
setCulling (ChangeCullEnabled False) = gl $ disable gl_CULL_FACE
setCulling (ChangeCullEnabled True) = gl $ enable gl_CULL_FACE
setCulling (ChangeCullFace face) =
        gl . GL.cullFace $ case face of
                                CullFront -> gl_FRONT
                                CullBack -> gl_BACK
                                CullFrontBack -> gl_FRONT_AND_BACK
setCulling _ = return ()

setDepthTest :: (GLES, MonadBase IO m) => DrawChange -> DrawT o m ()
setDepthTest (ChangeDepthTest True) = gl $ enable gl_DEPTH_TEST
setDepthTest (ChangeDepthTest False) = gl $ disable gl_DEPTH_TEST
setDepthTest _ = return ()

setDepthMask :: (GLES, MonadBase IO m) => DrawChange -> DrawT o m ()
setDepthMask (ChangeDepthMask True) = gl $ GL.depthMask true
setDepthMask (ChangeDepthMask False) = gl $ GL.depthMask false
setDepthMask _ = return ()

setColorMask :: (GLES, MonadBase IO m) => DrawChange -> DrawT o m ()
setColorMask (ChangeColorMask (r, g, b, a)) =
        gl $ GL.colorMask (bool r) (bool g) (bool b) (bool a)
        where bool True = true
              bool False = false
setColorMask _ = return ()

data Buffer = ColorBuffer | DepthBuffer | StencilBuffer deriving (Eq, Ord)

clearBuffers :: (GLES, MonadGL m) => [Buffer] -> m ()
clearBuffers = mapM_ $ gl . GL.clear . buffer
        where buffer ColorBuffer = gl_COLOR_BUFFER_BIT
              buffer DepthBuffer = gl_DEPTH_BUFFER_BIT
              buffer StencilBuffer = gl_STENCIL_BUFFER_BIT

createOutBuffer :: forall m o o'. (GLES, MonadBaseControl IO m)
                => Int
                -> Int
                -> OutBufferInfo o
                -> DrawT o' m (OutBuffer o)
createOutBuffer w h empty = 
        do let loader t = do bindTexture gl_TEXTURE_2D t
                             if pixelType == gl_FLOAT
                             then liftBase noFloat32Array >>=
                                          texImage2DFloat gl_TEXTURE_2D 0
                                                          internalFormat w' h'
                                                          0 format pixelType
                             else liftBase noUInt8Array >>=
                                          texImage2DUInt gl_TEXTURE_2D 0
                                                         internalFormat w' h'
                                                         0 format pixelType

           textures <- replicateM (fromIntegral texNum)
                                  (newTexture w h params cacheIdentifier loader)
           return $ case empty of
                         EmptyFloatGBuffer _ -> TextureFloatGBuffer w h textures
                         EmptyByteGBuffer _ -> TextureByteGBuffer w h textures
                         EmptyDepthBuffer _ ->
                                 TextureDepthBuffer w h $ head textures
                         EmptyDepthStencilBuffer _ ->
                                 TextureDepthStencilBuffer w h $ head textures
        where (w', h') = (fromIntegral w, fromIntegral h)
              cacheIdentifier = hash ( fromIntegral internalFormat :: Int
                                     , fromIntegral format :: Int
                                     , fromIntegral pixelType :: Int
                                     , params
                                     )
              (internalFormat, format, pixelType, params, texNum) =
                      case empty of
                           EmptyByteGBuffer params ->
                                   ( fromIntegral gl_RGBA
                                   , gl_RGBA
                                   , gl_UNSIGNED_BYTE
                                   , params
                                   , textureCount (Proxy :: Proxy o)
                                   )
                           EmptyFloatGBuffer params ->
                                   ( fromIntegral gl_RGBA32F
                                   , gl_RGBA
                                   , gl_FLOAT
                                   , params
                                   , textureCount (Proxy :: Proxy o)
                                   )
                           EmptyDepthBuffer params ->
                                   ( fromIntegral gl_DEPTH_COMPONENT
                                   , gl_DEPTH_COMPONENT
                                   , gl_UNSIGNED_SHORT
                                   , params
                                   , 1
                                   )
                           EmptyDepthStencilBuffer params ->
                                   ( fromIntegral gl_DEPTH_STENCIL
                                   , gl_DEPTH_STENCIL
                                   , gl_UNSIGNED_INT_24_8
                                   , params
                                   , 1
                                   )

-- TODO: spostare su OutBuffer?
drawBuffers' :: (GLES, FragmentShaderOutput o, MonadBaseControl IO m)
             => Int
             -> Int
             -> Bool
             -> Either (GBuffer o) (GBufferInfo o)
             -> Either DepthBuffer DepthBufferInfo
             -> DrawT o m a
             -> DrawT o' m (a, GBuffer o, DepthBuffer)
drawBuffers' w h addUnloader gBuffer depthBuffer draw =
        do (newColor, gBuffer') <-
                case gBuffer of
                     Right b -> (,) True <$> createOutBuffer w h b
                     Left b -> return (False, b)
           (newDepth, shouldClearStencil, depthBuffer') <-
                case depthBuffer of
                     Right b@(EmptyDepthBuffer _) ->
                             (,,) True False <$> createOutBuffer w h b
                     Right b@(EmptyDepthStencilBuffer _) ->
                             (,,) True True <$> createOutBuffer w h b
                     Left b -> return (False, False, b)
           Just ret <- drawUsedBuffers w h gBuffer' depthBuffer' . Just $
                   do when newColor clearColor
                      when newDepth clearDepth
                      when shouldClearStencil clearStencil
                      draw
           gl $ do when (addUnloader && newColor) $ bufferUnloader gBuffer'
                   when (addUnloader && newDepth) $ bufferUnloader depthBuffer'
           return (ret, gBuffer', depthBuffer')
        where bufferUnloader buf = 
                        mapM_ (unloader buf (Nothing :: Maybe TextureImage))
                              (textures buf)

drawUsedBuffers :: (GLES, MonadBase IO m)
                => Int
                -> Int
                -> GBuffer o
                -> DepthBuffer
                -> Maybe (DrawT o m a)
                -> DrawT o' m (Maybe a)
drawUsedBuffers w h gBuffer depthBuffer mdraw =
        do oldFb <- targetFrameBuffer <$> DrawT get
           oldDefaultFb <- defaultFrameBuffer <$> DrawT get
           oldViewport <- currentViewport
           oldDefaultViewport <- defaultViewport <$> DrawT get
           let oldFb' | isJust mdraw = oldFb <|> Just oldDefaultFb
                      | otherwise = Nothing
           ret <- drawToTextures useDrawBuffers attachments oldFb' $ \fb -> 
                case mdraw of
                     Nothing -> do DrawT . modify $
                                        \s -> s { targetFrameBuffer = Just fb }
                                   resizeViewport (0, 0) (w, h)
                                   return Nothing
                     Just (DrawT draw) ->
                        do resizeViewport (0, 0) (w, h)
                           es <- DrawT get
                           let es1 = es { targetFrameBuffer = Nothing
                                        , defaultFrameBuffer = fb
                                        , defaultViewport = ((0, 0), (w, h))
                                        , drawState =
                                            setTarget (noTarget DefaultTarget)
                                                      (drawState es)
                                        }
                           (ret, es2) <- DrawT . lift $ runStateT draw es1
                           let ds3 = case drawState es of
                                          DrawState {stateTarget = oldt} ->
                                                setTarget (noTarget oldt)
                                                          (drawState es2)
                               es3 = es2 { targetFrameBuffer = oldFb
                                         , defaultFrameBuffer = oldDefaultFb
                                         , defaultViewport = oldDefaultViewport
                                         , drawState = ds3
                                         }
                           DrawT $ put es3
                           uncurry resizeViewport oldViewport
                           return $ Just ret
           return ret
        where colorAttachments = zipWith (\(LoadedTexture _ _ _ t) n ->
                                                (t, gl_COLOR_ATTACHMENT0 + n)
                                         )
                                         (textures gBuffer)
                                         [0 ..]
              depthAttachment =
                      case depthBuffer of
                           TextureDepthBuffer _ _ (LoadedTexture _ _ _ t) ->
                                (t, gl_DEPTH_ATTACHMENT)
                           TextureDepthStencilBuffer _ _ (LoadedTexture _ _ _ t) ->
                                (t, gl_DEPTH_STENCIL_ATTACHMENT)
              attachments = depthAttachment : colorAttachments
              useDrawBuffers | (_ : _ : _) <- colorAttachments = True
                             | otherwise = False

deleteCurrentFrameBuffer :: (GLES, MonadBaseControl IO m) => DrawT o m ()
deleteCurrentFrameBuffer = do mfb <- targetFrameBuffer <$> DrawT get
                              case mfb of
                                   Just fb -> gl $ deleteFramebuffer fb
                                   Nothing -> return ()

resetFrameBuffer :: (GLES, MonadBaseControl IO m) => DrawT o m ()
resetFrameBuffer = do (sp, ss) <- defaultViewport <$> DrawT get
                      fb <- defaultFrameBuffer <$> DrawT get
                      resizeViewport sp ss
                      gl $ bindFramebuffer gl_FRAMEBUFFER fb
                      DrawT . modify $ \s -> s { targetFrameBuffer = Nothing }

drawToTextures :: (GLES, MonadScreen m, MonadGL m)
               => Bool
               -> [(GL.Texture, GLEnum)]
               -> Maybe FrameBuffer
               -> (FrameBuffer -> m a)
               -> m a
drawToTextures useDrawBuffers atts moldFb draw =
        do fb <- gl createFramebuffer 
           gl $ bindFramebuffer gl_FRAMEBUFFER fb

           buffersToDraw <- fmap concat . flip mapM atts $
                   \(t, attach) ->
                           do let drawAttachment =
                                   [ fromIntegral attach
                                   | attach /= gl_DEPTH_ATTACHMENT
                                   , attach /= gl_DEPTH_STENCIL_ATTACHMENT
                                   ]
                                        
                              gl $ framebufferTexture2D gl_FRAMEBUFFER attach
                                                        gl_TEXTURE_2D t 0
                              return drawAttachment

           when useDrawBuffers $
                   liftBase (encodeInts buffersToDraw) >>= gl . GL.drawBuffers

           ret <- draw fb

           case moldFb of
                Just oldFb -> gl $ do deleteFramebuffer fb
                                      bindFramebuffer gl_FRAMEBUFFER oldFb
                Nothing -> return ()

           return ret

newTexture :: (GLES, MonadBaseControl IO m)
           => Int
           -> Int
           -> TextureParameters
           -> Int
           -> (GL.Texture -> GL ())
           -> DrawT o m LoadedTexture
newTexture w h params i initialize =
        gl $ do t <- emptyTexture params
                initialize t
                return $ LoadedTexture (fromIntegral w) (fromIntegral h) i t

class ReadPixels r where
        readPixels :: MonadGL m => (Int, Int, Int, Int) -> GLEnum -> m r

instance GLES => ReadPixels [Color] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftBase . newUInt8Array $
                                        fromIntegral rw * fromIntegral rh * 4
                           gl $ readPixelsUInt8 (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_UNSIGNED_BYTE arr
                           liftBase $ fmap wordsToColors (decodeUInt8s arr)
                where wordsToColors (r : g : b : a : xs) =
                                Color r g b a : wordsToColors xs
                      wordsToColors _ = []

instance GLES => ReadPixels [Vec4] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftBase . newFloat32Array $
                                        fromIntegral rw * fromIntegral rh * 4
                           gl $ readPixelsFloat (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_FLOAT arr
                           liftBase $ fmap floatsToVecs (decodeFloat32s arr)
                where floatsToVecs (r : g : b : a : xs) =
                                Vec4 r g b a : floatsToVecs xs
                      floatsToVecs _ = []

instance GLES => ReadPixels [Word8] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftBase . newUInt8Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsUInt8 (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_UNSIGNED_BYTE arr
                           liftBase $ decodeUInt8s arr

instance GLES => ReadPixels [Word16] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftBase . newUInt16Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsUInt16 (fromIntegral x)
                                                 (fromIntegral y)
                                                 (fromIntegral rw)
                                                 (fromIntegral rh)
                                                 format gl_UNSIGNED_SHORT arr
                           liftBase $ decodeUInt16s arr

instance GLES => ReadPixels [Float] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftBase . newFloat32Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsFloat (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_FLOAT arr
                           liftBase $ decodeFloat32s arr
