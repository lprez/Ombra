{-# LANGUAGE CPP, TypeOperators, DataKinds, FlexibleContexts #-}

module Utils.OBJLoader (
        loadOBJ
) where

import Data.Maybe (maybe)
import Data.List
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Graphics.Rendering.Ombra (GLES)
import Graphics.Rendering.Ombra.Geometry as O
import Graphics.Rendering.Ombra.Shader.Default3D hiding (Attributes)
import Graphics.Rendering.Ombra.Vector
import Utils.OBJParser as P

#ifdef __GHCJS__
import qualified Data.JSString as JS
import JavaScript.Web.XMLHttpRequest
#else
import Data.Text.IO as T
#endif

geometry :: (GLES, Attributes (i ': is))
         => (Triangle (O.Vertex Geometry3D) -> Triangle (O.Vertex (i ': is)))
         -> [Command]
         -> Geometry (i ': is)
geometry trans = mkGeometry . reverse . fst7 . foldl' cmd initState
        where initState = ([], 1, H.empty, 1, H.empty, 1, H.empty)
              fst7 (x, _, _, _, _, _, _) = x
              rf = realToFrac
              vert (ls, us, ns) (P.Vertex l mu mn) =
                      Attr Position3 (ls H.! l) :~
                      Attr UV (maybe (Vec2 0 0) (us H.!) mu) :~
                      Attr Normal3 (maybe (Vec3 0 0 0) (ns H.!) mn)
              tris maps (P.Triangle x y z) = [ trans $ O.Triangle (vert maps x)
                                                                  (vert maps y)
                                                                  (vert maps z)
                                             ]
              tris maps (Quad x y z w) = tris maps (P.Triangle x y z) ++
                                         tris maps (P.Triangle x z w)
              cmd (ts, nls, ls, nus, us, nns, ns)
                  (VertexLocationCommand (VertexLocation x y z)) =
                        ( ts
                        , nls + 1, H.insert nls (Vec3 (rf x) (rf y) (rf z)) ls
                        , nus, us
                        , nns, ns
                        )
              cmd (ts, nls, ls, nus, us, nns, ns)
                  (VertexUVCommand (VertexUV u v)) =
                        ( ts
                        , nls, ls
                        , nus + 1, H.insert nus (Vec2 (rf u) (rf v)) us
                        , nns, ns
                        )
              cmd (ts, nls, ls, nus, us, nns, ns)
                  (VertexNormalCommand (VertexNormal x y z)) =
                        ( ts
                        , nls, ls
                        , nus, us
                        , nns + 1, H.insert nns (Vec3 (rf x) (rf y) (rf z)) ns
                        )
              cmd (ts, nls, ls, nus, us, nns, ns) (FaceCommand face) =
                      ( tris (ls, us, ns) face ++ ts
                      , nls, ls
                      , nus, us
                      , nns, ns
                      )
              cmd s _ = s

loadOBJ :: (GLES, Attributes (i ': is))
        => (Triangle (O.Vertex Geometry3D) -> Triangle (O.Vertex (i ': is)))
        -> FilePath
        -> IO (Either String (Geometry (i ': is)))
loadOBJ transform path = downloadOBJ path >>= \etxt -> return $
        case etxt of
             Left s -> Left s
             Right txt -> case parseOBJ txt of
                               Right obj -> Right $ geometry transform obj
                               Left s -> Left s

downloadOBJ :: FilePath -> IO (Either String Text)
#ifdef __GHCJS__
downloadOBJ fpath = xhrText req >>= \resp -> return $
        case resp of
             Response (Just text) 200 _ _ -> Right text
             Response _ status _ _ -> Left $ "Status: " ++ show status
        where req = Request { reqMethod = GET
                            , reqURI = JS.pack fpath
                            , reqLogin = Nothing
                            , reqHeaders = []
                            , reqWithCredentials = False
                            , reqData = NoData
                            }
#else
downloadOBJ = fmap Right . T.readFile
#endif
