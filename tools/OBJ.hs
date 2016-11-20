module Main where

import qualified Data.IntMap as M
import Data.Maybe
import Data.Vect.Float
import qualified Data.Vector as V
import Codec.Wavefront -- From the 'wavefront' package
import System.Environment

main :: IO ()
main = do (file : _) <- getArgs
          eobj <- fromFile $ file
          case eobj of
               Right obj -> let (es, vs) = convert obj
                            in do putStr $ "mkGeometry3DInd "
                                  putStrLn . prettyList "\n                " $
                                          map show vs
                                  putStr "                "
                                  putStrLn . prettyList "\n                " $
                                          showTriangles es
               Left err -> putStrLn err

prettyList :: String -> [String] -> String
prettyList ident (x : xs) = "[ " ++ x ++
                            concatMap (\x -> ident ++ ", " ++ x) xs ++
                            ident ++ "]"

showTriangles :: [Int] -> [String]
showTriangles [] = []
showTriangles (x : y : z : ts) = ("Triangle " ++ show x ++ " "
                                              ++ show y ++ " "
                                              ++ show z) : showTriangles ts

convert :: WavefrontOBJ -> ([Int], [(Vec3, Vec2, Vec3)])
convert obj =
        let locations = objLocations obj
            lslen = V.length locations

            texCoords = objTexCoords obj `V.snoc` TexCoord 0 0 0
            tslen = V.length texCoords
        
            normals = objNormals obj `V.snoc` Normal 0 0 0
            nslen = V.length normals

            build (FaceIndex locIdx tcIdx normIdx)
                  (attrMap, elemList, vertList, nextAttrIdx) =
                let locIdx' = locIdx - 1
                    tcIdx' = maybe tslen id tcIdx - 1
                    normIdx' = maybe nslen id normIdx - 1
                    mapIdx = locIdx' * nslen * tslen + tcIdx' * nslen + normIdx'
                in case M.lookup mapIdx attrMap of
                        Just attrIdx -> ( attrMap
                                        , attrIdx : elemList
                                        , vertList
                                        , nextAttrIdx )
                        Nothing -> ( M.insert mapIdx nextAttrIdx attrMap
                                   , nextAttrIdx : elemList
                                   , ( location (locations V.! locIdx')
                                     , texCoord (texCoords V.! tcIdx')
                                     , normal (normals V.! normIdx')
                                     ) : vertList
                                   , nextAttrIdx + 1)

            buildFace (Triangle f1 f2 f3) = build f1 . build f2 . build f3
            buildFace _ = error "Only triangles are supported."

            (_, es, vs, _) = V.foldr (\elem -> buildFace $ elValue elem)
                                     (M.empty, [], [], 0)
                                     (objFaces obj)
        in (es, reverse vs)

location :: Location -> Vec3
location (Location x y z _) = Vec3 x y z

texCoord :: TexCoord -> Vec2
texCoord (TexCoord r s _) = Vec2 r s

normal :: Normal -> Vec3
normal (Normal x y z) = Vec3 x y z
