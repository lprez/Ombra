{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes, FlexibleContexts,
             KindSignatures, DataKinds, TypeOperators, ConstraintKinds #-}

module Graphics.Rendering.Ombra.Shader.GLSL (
        vertexToGLSLAttr,
        vertexToGLSL,
        fragmentToGLSL,
        shaderToGLSL,
        globalName,
        attributeName
) where

import Control.Monad
import Data.Hashable (hash)
import qualified Data.HashMap.Strict as H
import Data.Typeable
import Graphics.Rendering.Ombra.Shader.Shader
import Graphics.Rendering.Ombra.Shader.Language.Types hiding (Int, Bool)
import Graphics.Rendering.Ombra.Shader.Stages (VertexShader, FragmentShader, ValidVertex)
import Text.Printf

type ShaderVars = ( [(String, String)]
                  , [(String, String, Int)]
                  , [(String, String, Expr)])

vertexToGLSLAttr :: ValidVertex g i o => VertexShader g i o
                 -> (String, [(String, Int)])
vertexToGLSLAttr v =
        let r@(_, is, _) = vars False v
        in ( shaderToGLSL "#version 100\n" "attribute" "varying"
                          r [("hvVertexShaderOutput", "gl_Position")]
           , map (\(t, n, s) -> (n, s)) is)

vertexToGLSL :: ValidVertex g i o => VertexShader g i o -> String
vertexToGLSL = fst . vertexToGLSLAttr

fragmentToGLSL :: Valid g i '[] => FragmentShader g i -> String
fragmentToGLSL v = shaderToGLSL "#version 100\nprecision mediump float;"
                                "varying" ""
                                (vars True v)
                                [("hvFragmentShaderOutput", "gl_FragColor")]

shaderToGLSL :: String -> String -> String -> ShaderVars -> [(String, String)] -> String
shaderToGLSL header ins outs (gs, is, os) predec = concat
        [ header
        , concatMap (var "uniform") gs
        , concatMap (\(t, n, _) -> var ins (t, n)) is
        , concatMap (\(t, n, _) -> if any ((== n) . fst) predec
                                          then []
                                          else var outs (t, n)
                    ) os
        , "void main(){"
        , actions
        , concatMap (\(n, s) -> replace n predec ++ "=" ++ s ++ ";")
                    compiledOuts
        , "}" ]
        where var qual (ty, nm) = qual ++ " " ++ ty ++ " " ++ nm ++ ";"
              replace x xs = case filter ((== x) . fst) xs of
                                        ((_, y) : []) -> y
                                        _ -> x
              (_, outNames, outExprs) = unzip3 os
              (actions, outStrs) = compile outExprs
              compiledOuts = zip outNames outStrs

vars :: Valid gs is os => Bool -> Shader gs is os -> ShaderVars
vars isFragment (shader :: Shader gs is os) =
        ( staticList (undefined :: Proxy gs) globalTypeAndName
        , staticList (undefined :: Proxy is) inputVar
        , stFold (\acc x -> outputVar x : acc) [] outputs)
        where outputs = shader (staticSTList (undefined :: Proxy gs) globalExpr)
                               (staticSTList (undefined :: Proxy is) inputExpr)

              globalExpr, inputExpr :: (Typeable x, ShaderType y) => x -> y
              globalExpr x = fromExpr . Read $ globalName x
              inputExpr x = fromExpr . Read $ if isFragment then varyingName x
                                               else attributeName x

              inputVar :: (Typeable x, ShaderType x)
                       => x -> (String, String, Int)
              inputVar x = let (ty, nm) = if isFragment
                                                then varyingTypeAndName x
                                                else attributeTypeAndName x
                           in (ty, nm, size x)

              outputVar :: (Typeable x, ShaderType x)
                        => x -> (String, String, Expr)
              outputVar x = let (ty, nm) = varyingTypeAndName x
                            in (ty, nm, toExpr x)

type ActionID = Int
type ActionMap = H.HashMap ActionID Action
type ActionSet = H.HashMap ActionID ()

data ActionInfo = ActionInfo {
        actionGenerator :: ActionGenerator,
        actionDeps :: ActionSet,
        actionContext :: ActionContext
}

type ActionGenerator = String -> String

-- | The context is where an action should be put. Only for-loops are considered
-- contexts, because there is no reason to put an action inside another block.
-- Of course, an action could have many contexts (e.g. for(..) { for (..) {
-- act; } }), but only one is actually needed to compile the action.
data ActionContext = ShallowContext ActionSet       -- ^ The contexts of the expressions used in the action.
                   | DeepContext ActionSet          -- ^ All the contexts (including those of the dependencies).
                   deriving (Show)

type ActionGraph = H.HashMap ActionID ActionInfo

-- | Compile a list of 'Expr', sharing their actions.
compile :: [Expr] -> (String, [String])
compile exprs = let (strs, deps, _) = unzip3 $ map compileExpr exprs
                    depGraph = contextAll deep . buildActionGraph $ H.unions deps
                    sorted = sortActions depGraph
                in (sorted >>= uncurry generate, strs)

generate :: ActionGenerator -> ActionGraph -> String
generate gen graph = gen $ sortActions graph >>= uncurry generate

sortActions :: ActionGraph -> [(ActionGenerator, ActionGraph)]
sortActions fullGraph = visitLoop (H.empty, [], fullGraph)
        where visitLoop state@(childrenMap, sortedIDs, graph)
                | H.null graph = map (makePair childrenMap fullGraph) sortedIDs
                | otherwise = visitLoop $ visit (head $ H.keys graph) state

              visit aID state@(_, _, graph) =
                      case H.lookup aID graph of
                              Nothing -> state
                              Just ai -> visitNew aID ai state

              visitNew aID ai (childrenMap, sortedIDs, graph) = 
                      let deps = actionDeps ai
                          (childrenMap', sortedIDs', graph') =
                                H.foldrWithKey
                                        (\aID _ state -> visit aID state)
                                        (childrenMap, sortedIDs, graph)
                                        deps
                      in case actionContext ai of
                              DeepContext ctx | H.null ctx ||
                                                ctx == H.singleton aID () ->
                                         ( childrenMap', sortedIDs' ++ [aID]
                                         , H.delete aID graph' )

                              DeepContext ctx ->
                                      let smap = H.map (\_ -> H.singleton aID ai
                                                       ) ctx
                                          cmap' = H.unionWith H.union smap
                                                              childrenMap'
                                      in (cmap', sortedIDs', H.delete aID graph')

              makePair childrenMap graph aID = 
                      ( actionGenerator $ graph H.! aID
                      , case H.lookup aID childrenMap of
                             Just g -> H.map (delDeep aID) g
                             Nothing -> H.empty )

              delDeep k ai = let (DeepContext ctx) = actionContext ai
                             in ai { actionContext = DeepContext $
                                                        H.delete k ctx }

-- | Build an action graph with shallow contexts.
buildActionGraph :: ActionMap -> ActionGraph
buildActionGraph = flip H.foldrWithKey H.empty $
        \aID act graph ->
                let (info, deps) = compileAction aID act
                in H.union (H.insert aID info graph)
                           (buildActionGraph deps)

-- | Transform every context.
contextAll :: (ActionID -> ActionGraph -> (ActionContext, ActionGraph))
           -> ActionGraph -> ActionGraph
contextAll f g = H.foldrWithKey (\aID _ graph -> snd $ f aID graph) g g

-- | Find and build the deep context of this action. Returns a deep context and
-- a new graph with the deep contexts of this action and of its dependencies.
deep :: ActionID -> ActionGraph -> (ActionContext, ActionGraph)
deep aID graph =
        case actionContext act of
                ShallowContext sctx ->
                        let (dctx, graph') = H.foldrWithKey addDepContext
                                                            (sctx, graph)
                                                            (actionDeps act)
                            ctx' = DeepContext dctx
                        in (ctx', H.insert
                                      aID (act { actionContext = ctx' }) graph')
                ctx -> (ctx, graph)
        where act = graph H.! aID
              addDepContext depID depInfo (ctx, graph) = 
                      let (DeepContext dCtx, graph') = deep depID graph 
                      in (H.union ctx (H.delete depID dCtx), graph')

-- | Compile an 'Expr'. Returns the compiled expression, the map of dependencies
-- and the context.
compileExpr :: Expr -> (String, ActionMap, ActionSet)
compileExpr Empty = ("", H.empty, H.empty)
compileExpr (Read s) = (s, H.empty, H.empty)

compileExpr (Op1 s e) = first3 (\x -> "(" ++ s ++ x ++ ")") $ compileExpr e

compileExpr (Op2 s ex ey) = let (x, ax, cx) = compileExpr ex
                                (y, ay, cy) = compileExpr ey
                            in ( "(" ++ x ++ s ++ y ++ ")"
                               , H.union ax ay, H.union cx cy )

compileExpr (Apply s es) = let (vs, as, cs) = unzip3 $ map compileExpr es
                           in ( concat $ [ s, "(" , tail (vs >>= (',' :)), ")" ]
                              , H.unions as, H.unions cs)

compileExpr (X e) = first3 (++ "[0]") $ compileExpr e
compileExpr (Y e) = first3 (++ "[1]") $ compileExpr e
compileExpr (Z e) = first3 (++ "[2]") $ compileExpr e
compileExpr (W e) = first3 (++ "[3]") $ compileExpr e
compileExpr (Literal s) = (s, H.empty, H.empty)
compileExpr (Action a) = let h = hash a
                         in (actionName h, H.singleton h a, H.empty)
compileExpr (Dummy _) = error "compileExpr: Dummy"
compileExpr (ArrayIndex eArr ei) = let (arr, aArr, cArr) = compileExpr eArr
                                       (i, ai, ci) = compileExpr ei
                                   in ( "(" ++ arr ++ "[" ++ i ++ "])"
                                      , H.union aArr ai, H.union cArr ci )
compileExpr (ContextVar i t) = (contextVarName t i, H.empty, H.singleton i ())

first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f (a, b, c) = (f a, b, c)

compileAction :: ActionID -> Action -> (ActionInfo, ActionMap)
compileAction aID (Store ty expr) =
        let (eStr, deps, ctxs) = compileExpr expr
        in ( ActionInfo  (\c -> concat [ c, ty, " ", actionName aID
                                       , "=", eStr, ";" ])
                         (H.map (const ()) deps)
                         (ShallowContext ctxs)
           , deps )

compileAction aID (If cExpr ty tExpr fExpr) =
        let (cStr, cDeps, cCtxs) = compileExpr cExpr
            (tStr, tDeps, tCtxs) = compileExpr tExpr
            (fStr, fDeps, fCtxs) = compileExpr fExpr
            deps = H.unions [cDeps, tDeps, fDeps]
            name = actionName aID
        in ( ActionInfo (\c -> concat [ ty, " ", name, ";if("
                                      , cStr, "){", c, name, "=", tStr
                                      , ";}else{" , name, "=", fStr, ";}" ])
                        (H.map (const ()) deps)
                        (ShallowContext $ H.unions [cCtxs, tCtxs, fCtxs])
           , deps )

compileAction aID (For iters ty initVal body) =
        let iterName = contextVarName LoopIteration aID
            valueName = contextVarName LoopValue aID
            (nExpr, sExpr) = body (ContextVar aID LoopIteration)
                                  (ContextVar aID LoopValue)
            (iStr, iDeps, iCtxs) = compileExpr initVal
            (nStr, nDeps, nCtxs) = compileExpr nExpr
            (sStr, sDeps, sCtxs) = compileExpr sExpr
            deps = H.unions [iDeps, nDeps, sDeps]
        in ( ActionInfo (\c -> concat [ ty, " ", valueName, "=", iStr, ";"
                                      , "for(float ", iterName, "=0.0;"
                                      , iterName, "<", show iters, ".0;"
                                      , "++", iterName, "){", c
                                      , "if(", sStr, "){break;}"
                                      , valueName, "=", nStr, ";}" ])
                        (H.map (const ()) deps)
                        (ShallowContext $ H.unions [iCtxs, nCtxs, sCtxs])
           , deps )

actionName :: ActionID -> String
actionName = ('a' :) . hashName

contextVarName :: ContextVarType -> ActionID -> String
contextVarName LoopIteration = ('l' :) . hashName
contextVarName LoopValue = actionName

hashName :: ActionID -> String
hashName = printf "%x"

globalTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
globalTypeAndName t = (typeName t, globalName t)

varyingTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
varyingTypeAndName t = (typeName t, varyingName t)

attributeTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
attributeTypeAndName t = (typeName t, attributeName t)

variableName :: Typeable t => t -> String
variableName = tyConName . typeRepTyCon . typeOf

globalName :: Typeable t => t -> String
globalName = ("hg" ++) . variableName

varyingName :: Typeable t => t -> String
varyingName = ("hv" ++) . variableName

attributeName :: Typeable t => t -> String
attributeName = ("ha" ++) . variableName
