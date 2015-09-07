module Graphics.Rendering.Ombra.Geometry.OBJ where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST
import Data.Vect.Float
import qualified Data.Vector.Storable as V
import Data.STRef
import Data.Word (Word16)

import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Internal.STVectorLen
import Graphics.Rendering.Ombra.Geometry

data OBJModel = OBJModel {
        objVertices :: V.Vector Vec3,
        objUVCoords :: V.Vector Vec2,
        objNormals :: V.Vector Vec3,
        objFaces :: [[(Int, Int, Int)]]
} deriving (Show)

parseOBJ :: String -> OBJModel
parseOBJ file = runST $
        do vs <- new
           us <- new
           ns <- new
           fs <- newSTRef []

           flip mapM_ (lines file) $ \ l -> case l of
                   ('v' : ' ' : v3) -> cons (parseVec3 v3) vs
                   ('v' : 't' : ' ' : v2) -> cons (parseVec2 v2) us
                   ('v' : 'n' : ' ' : v3) -> cons (parseVec3 v3) ns
                   ('f' : ' ' : f) -> modifySTRef fs (parseFace f :)
                   _ -> return ()

           usLen <- readSTRef $ snd us
           nsLen <- readSTRef $ snd ns

           when (usLen <= 0) $ cons (Vec2 0 0) us
           when (nsLen <= 0) $ cons (Vec3 0 0 0) ns

           OBJModel <$> freeze vs
                    <*> freeze us
                    <*> freeze ns
                    <*> readSTRef fs

        where split s e str = iter str "" []
                where iter (x : xs) cur fnd | x == s = iter xs "" $ 
                                                           reverse cur : fnd
                                            | x /= e = iter xs (x : cur) fnd
                      iter _ [] fnd = fnd
                      iter _ cur fnd = reverse cur : fnd
              parseVec3 str = case split ' ' '#' str of
                                (z : y : x : _) -> Vec3 (parseFloat x) 
                                                      (parseFloat y)
                                                      (parseFloat z)
                                _ -> error "parseOBJ: invalid vertex/normal"
              parseVec2 str = case split ' ' '#' str of
                                (y : x : _) -> Vec2 (parseFloat x)
                                                    (parseFloat y)
                                _ -> error "parseOBJ: invalid uv coordinate"
              parseFace = map parseElement . reverse . split ' ' '#'

              parseElement str = case split '/' ' ' str of
                                     (n : t : v : _) -> ( parseInt v - 1
                                                        , parseInt t - 1
                                                        , parseInt n - 1 )
                                     _ -> error "parseOBJ: invalid element"

              parseInt [] = 1
              parseInt s = read s

              parseFloat [] = 0
              parseFloat s = read s

attributesOBJ :: OBJModel -> ([Vec3], [Vec2], [Vec3], [Word16])
attributesOBJ (OBJModel v u n fs) = arraysToElements $ facesToArrays v u n fs

geometryOBJ :: GLES => OBJModel -> Geometry Geometry3D
geometryOBJ o = let (v, u, n, e) = attributesOBJ o in mkGeometry3D v u n e
