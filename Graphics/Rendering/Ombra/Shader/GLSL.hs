{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes, FlexibleContexts,
             DataKinds, TypeOperators, ConstraintKinds #-}

module Graphics.Rendering.Ombra.Shader.GLSL (
        compileVertexShader,
        compileFragmentShader,
        uniformName
) where

import Control.Arrow
import Data.List (find)
import Data.Hashable (hash)
import qualified Data.HashMap.Strict as H
import Data.Typeable
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.Types
import Text.Printf

-- TODO: IntMap instead of HashMap
-- TODO: use DList/ShowS
-- TODO: use fgl
-- TODO: detect identical uniforms

compileVertexShader :: (ShaderInput i, MultiShaderType o)
                    => VertexShader i (GVec4, o)
                    -> (String, (UniformID, [(String, Int)]))
compileVertexShader s = compileShader True header ["gl_Position"] 0 exprShader
        where exprShader = s >>^ \(pos, outs) -> ([toExpr pos], outExprs outs)
              outExprs = foldrMST (\x l -> (typeName x, toExpr x) : l) []
              header = "#version 100\nprecision mediump float;"

compileFragmentShader :: (ShaderInput i, FragmentShaderOutput o)
                      => UniformID
                      -> FragmentShader i o
                      -> String
compileFragmentShader i s = fst $ compileShader False header outs i exprShader
        where exprShader = s >>^ flip (,) [] . map toExpr . toGVec4s
              header = concat [ "#version 100\n"
                              , ext "GL_EXT_draw_buffers"
                              , ext "GL_OES_standard_derivatives"
                              , "precision mediump float;"
                              ]
              ext e = "#extension " ++ e ++ " : enable\n"
              outs = map (\n -> "gl_FragData[" ++ show n ++ "]") [0 .. 15]

compileShader :: ShaderInput i
              => Bool
              -> String
              -> [String]
              -> UniformID
              -> Shader s i ([Expr], [(String, Expr)])
              -> (String, (UniformID, [(String, Int)]))
compileShader useAttributes header outNames uniformID (Shader shaderFun _) =
        let inputFun | useAttributes = Attribute
                     | otherwise = Input
            (input, _) = buildMST (fromExpr . inputFun) 0
            inputTypes = foldrMST (\x -> (typeName x :)) [] input
            ((ShaderState uniformID' uniMap _), (outs, outVaryings)) =
                    shaderFun (ShaderState uniformID [] [], input)
            (outVaryingTypes, outVaryingExprs) = unzip outVaryings

            showVar qual ty nm = concat [qual, " ", ty, " ", nm, ";"]
            showVars qual name vars = concatMap (\(ty, n) -> showVar qual ty
                                                                     (name n))
                                                (zip vars [0 ..])
            uniType (UniformValue (_ :: Proxy g) _) = typeName (undefined :: g)
            uniType (UniformTexture _) = typeName (undefined :: GSampler2D)
            uniVars = showVars "uniform" uniformName $ map (uniType . snd) uniMap
            inputVars | useAttributes = showVars "attribute" attributeName
                                                 inputTypes
                      | otherwise = showVars "varying" varyingName inputTypes
            outVaryingVars = showVars "varying" varyingName outVaryingTypes

            outVaryingNames = map varyingName [0 ..]
            (compiledActions, outStrs) = compile (outs ++ outVaryingExprs)
            compiledOuts = concat $ zipWith (\n s -> concat [n, "=", s, ";"])
                                            (outNames ++ outVaryingNames)
                                            outStrs
        in ( concat [ header
                    , uniVars
                    , inputVars
                    , outVaryingVars
                    , "void main(){"
                    , compiledActions
                    , compiledOuts
                    , "}"
                    ]
            , ( uniformID'
              , if useAttributes
                   then zipWith (\size n -> (attributeName n, size))
                               (foldrMST (\x -> (size x :)) [] input)
                               [0 ..]
                   else []
              )
            )

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
                                        (\aID' _ state -> visit aID' state)
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
                              ShallowContext _ ->
                                      error "sortActions: unexpected \
                                            \ShallowContext"

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
              addDepContext depID _ (ctx, graph) = 
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
compileExpr (Input i) = (varyingName i, H.empty, H.empty)
compileExpr (Attribute i) = (attributeName i, H.empty, H.empty)
compileExpr (Uniform i) = (uniformName i, H.empty, H.empty)
compileExpr (Action a) = let h = hash a
                         in (actionName h, H.singleton h a, H.empty)
compileExpr (Dummy _) = error "compileExpr: Dummy"
compileExpr (HashDummy _) = error "compileExpr: HashDummy"
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

uniformName :: Int -> String
uniformName = ('u' :) . show

varyingName :: Int -> String
varyingName = ('v' :) . show

attributeName :: Int -> String
attributeName = ('t' :) . show
