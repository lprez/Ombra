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
               Right obj -> let (es, ls, ts, ns) = convert obj
                            in do putStr $ "mkGeometry3D "
                                  putStrLn $ prettyList "\n             " ls
                                  putStr "             "
                                  putStrLn $ prettyList "\n             " ts
                                  putStr "             "
                                  putStrLn $ prettyList "\n             " ns
                                  putStr "             "
                                  putStrLn $ prettyList "\n             " es
               Left err -> putStrLn err

prettyList :: Show a => String -> [a] -> String
prettyList ident (x : xs) = "[ " ++ show x ++
                            concatMap (\x -> ident ++ ", " ++ show x) xs ++
                            ident ++ "]"

convert :: WavefrontOBJ -> ([Int], [Vec3], [Vec2], [Vec3])
convert obj =
        let locations = objLocations obj
            lslen = V.length locations

            texCoords = objTexCoords obj `V.snoc` TexCoord 0 0 0
            tslen = V.length texCoords
        
            normals = objNormals obj `V.snoc` Normal 0 0 0
            nslen = V.length normals

            build (FaceIndex locIdx tcIdx normIdx)
                  (attrMap, elemList, locList, tcList, normList, nextAttrIdx) =
                let locIdx' = locIdx - 1
                    tcIdx' = maybe tslen id tcIdx - 1
                    normIdx' = maybe nslen id normIdx - 1
                    mapIdx = locIdx' * nslen * tslen + tcIdx' * nslen + normIdx'
                in case M.lookup mapIdx attrMap of
                        Just attrIdx -> ( attrMap
                                        , attrIdx : elemList
                                        , locList
                                        , tcList
                                        , normList
                                        , nextAttrIdx )
                        Nothing -> ( M.insert mapIdx nextAttrIdx attrMap
                                   , nextAttrIdx : elemList
                                   , location (locations V.! locIdx') : locList
                                   , texCoord (texCoords V.! tcIdx') : tcList
                                   , normal (normals V.! normIdx') : normList
                                   , nextAttrIdx + 1)

            buildFace (Triangle f1 f2 f3) = build f1 . build f2 . build f3
            buildFace _ = error "Only triangles are supported."

            (_, es, ls, ts, ns, _) = V.foldr (\elem -> buildFace $ elValue elem)
                                             (M.empty, [], [], [], [], 0)
                                             (objFaces obj)
        in (es, reverse ls, reverse ts, reverse ns)

location :: Location -> Vec3
location (Location x y z _) = Vec3 x y z

texCoord :: TexCoord -> Vec2
texCoord (TexCoord r s _) = Vec2 r s

normal :: Normal -> Vec3
normal (Normal x y z) = Vec3 x y z
