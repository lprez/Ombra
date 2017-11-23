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
-- TODO: use fgl
-- TODO: detect identical uniforms

compileVertexShader :: (ShaderInput i, ShaderInput o)
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
            (input, _) = buildMST' (\t -> fromExpr . inputFun t) 0
            inputTypes = foldrMST (\x -> (typeName x :)) [] input
            ((ShaderState uniformID' uniMap _), (outs, outVaryings)) =
                    shaderFun (ShaderState uniformID [] [], input)
            (outVaryingTypes, outVaryingExprs) = unzip outVaryings

            showVar qual ty nm = concat [qual, " ", ty', " ", nm', ";"]
                where (ty', arr) = break (== '[') ty
                      nm' = nm ++ arr
            showVars qual name vars = concatMap (\(ty, n) -> showVar qual ty
                                                                     (name n))
                                                (zip vars [0 ..])
            uniType (UniformValue (_ :: Proxy g) _) = typeName (undefined :: g)
            uniType (UniformTexture _) = typeName (undefined :: GSampler2D)
            uniVars = concatMap (\(uid, val) -> showVar "uniform"
                                                        (uniType val)
                                                        (uniformName uid))
                                uniMap
            inputVars | useAttributes = showVars "attribute" attributeName
                                                 inputTypes
                      | otherwise = showVars "varying" varyingName inputTypes
            outVaryingVars = showVars "varying" varyingName outVaryingTypes

            outVaryingNames = map varyingName [0 ..]
            (compiledActions, outStrs) = compile (outs ++ outVaryingExprs)
            compiledOuts = zipWith (\n s -> showString (n ++ "=") . s .
                                            showString ";")
                                   (outNames ++ outVaryingNames)
                                   outStrs
        in ( concat [ header
                    , uniVars
                    , inputVars
                    , outVaryingVars
                    , "void main(){"
                    , compiledActions ""
                    , foldr (.) id compiledOuts $ ""
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

type ActionGenerator = ShowS -> ShowS

data ActionContext = ShallowContext ActionSet       -- ^ The contexts of the expressions used in the action.
                   | DeepContext ActionSet          -- ^ All the contexts (including those of the dependencies).
                   deriving (Show)

type ActionGraph = H.HashMap ActionID ActionInfo

-- | Compile a list of 'Expr', sharing their actions.
compile :: [Expr] -> (ShowS, [ShowS])
compile exprs = let exprs' = optimize 1 exprs
                    (strs, deps, _) = unzip3 $ map compileExpr exprs'
                    depGraph = contextAll deep . buildActionGraph $ H.unions deps
                    sorted = sortActions depGraph
                in (genList sorted, strs)

optimize :: Int         -- ^ 1 = store only the repeated expressions,
                        --   2 = store everything
         -> [Expr]
         -> [Expr]
optimize n exprs = let reps fullExprMap expr (optimizedExprs, exprMap) =
                        let h = hash expr
                            exprMap' = H.insertWith (+) h n exprMap
                            sub = subExprs expr
                            (optimizedSubExprs, exprMap'') =
                                    foldr (reps fullExprMap)
                                          ([], exprMap')
                                          sub
                            expr' = replaceSubExprs expr optimizedSubExprs
                            storedExpr = case exprType expr' of
                                              Just t -> Action (Store t expr') 0
                                              Nothing -> expr'
                            optimizedExpr | length sub < 1 = expr'
                                          | fullExprMap H.! h > 1 = storedExpr
                                          | otherwise = expr'
                            optimizedExprs' = optimizedExpr : optimizedExprs
                        in (optimizedExprs', exprMap'')
                       (optimizedExprs, fullExprMap) =
                               foldr (reps fullExprMap) ([], H.empty) exprs
                   in optimizedExprs

generate :: ActionGenerator -> ActionGraph -> ShowS
generate gen graph = gen . genList $ sortActions graph

genList :: [(ActionGenerator, ActionGraph)] -> ShowS
genList = foldr (.) id . map (uncurry generate)

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
compileExpr :: Expr -> (ShowS, ActionMap, ActionSet)
compileExpr Empty = (showString "", H.empty, H.empty)
compileExpr (Read _ s) = (showString s, H.empty, H.empty)

compileExpr (Op1 _ s e) = let left = showString ("(" ++ s)
                              right = showString ")"
                          in first3 (\x -> left . x . right) $ compileExpr e

compileExpr (Op2 _ s ex ey) = let (x, ax, cx) = compileExpr ex
                                  (y, ay, cy) = compileExpr ey
                                  str = showString "(" .
                                        x . showString s . y .
                                        showString ")"
                              in (str, H.union ax ay, H.union cx cy)

compileExpr (Apply _ s es) = let (v : vs, as, cs) = unzip3 $ map compileExpr es
                                 addArg vs v = vs . showString "," . v
                                 args = v . foldl addArg id vs
                                 str = showString (s ++ "(") . args . showString ")"
                             in (str, H.unions as, H.unions cs)

compileExpr (X e) = first3 (. showString "[0]") $ compileExpr e
compileExpr (Y e) = first3 (. showString "[1]") $ compileExpr e
compileExpr (Z e) = first3 (. showString "[2]") $ compileExpr e
compileExpr (W e) = first3 (. showString "[3]") $ compileExpr e
compileExpr (Literal _ s) = (showString s, H.empty, H.empty)
compileExpr (Input _ i) = (showString $ varyingName i, H.empty, H.empty)
compileExpr (Attribute _ i) = (showString $ attributeName i, H.empty, H.empty)
compileExpr (Uniform _ i) = (showString $ uniformName i, H.empty, H.empty)
compileExpr (Action a n) = let h = hash a
                               var = showString $ actionName h n
                           in (var, H.singleton h a, H.empty)
compileExpr (Dummy _) = error "compileExpr: Dummy"
compileExpr (HashDummy _) = error "compileExpr: HashDummy"
compileExpr (ArrayIndex _ eArr ei) = let (arr, aArr, cArr) = compileExpr eArr
                                         (i, ai, ci) = compileExpr ei
                                         str = showString "(" . arr .
                                               showString "[" . i .
                                               showString "])"
                                     in (str, H.union aArr ai, H.union cArr ci)
compileExpr (ContextVar _ i t) = ( showString $ contextVarName t i
                                 , H.empty
                                 , H.singleton i ()
                                 )

compileExprOptimized :: Int -> Expr -> (ShowS, ActionMap, ActionSet)
compileExprOptimized n = compileExpr . head . optimize n . (: [])

first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f (a, b, c) = (f a, b, c)

compileAction :: ActionID -> Action -> (ActionInfo, ActionMap)
compileAction aID (Store ty expr) =
        let (eStr, deps, ctxs) = compileExpr expr
        in ( ActionInfo  (\c -> c .
                                showString (ty ++ " " ++ actionName aID 0 ++ "=") .
                                eStr . showString ";")
                         (H.map (const ()) deps)
                         (ShallowContext ctxs)
           , deps )

compileAction aID (If cExpr ty tExpr fExpr) =
        let (cStr, cDeps, cCtxs) = compileExprOptimized 1 cExpr
            (tStr, tDeps, tCtxs) = compileExprOptimized 1 tExpr
            (fStr, fDeps, fCtxs) = compileExprOptimized 1 fExpr
            deps = H.unions [cDeps, tDeps, fDeps]
            name = actionName aID 0
            nameS = showString name
        in ( ActionInfo (\c -> showString (ty ++ " " ++ name ++ ";if(") .
                               cStr . showString "){" . c . nameS .
                               showString "=" . tStr . showString ";}else{" .
                               nameS . showString "=" . fStr . showString ";}")
                        (H.map (const ()) deps)
                        (ShallowContext $ H.unions [cCtxs, tCtxs, fCtxs])
           , deps )

compileAction aID (For iters initValuesTypes (ForBody body)) =
        let iterNameS = showString $ contextVarName LoopIteration aID
            valueNameSs = map (\i -> showString $ contextVarName (LoopValue i)
                                                                 aID)
                              [0 ..]
            (iterStr, _, _) = compileExpr iters
            Just iterType = exprType iters
            iterTypeS = showString iterType
            (initTypes, initValues) = unzip initValuesTypes
            (nExprs, sExpr) = body (ContextVar iterType aID LoopIteration)
                                   (zipWith (\ty i -> ContextVar ty aID
                                                                 (LoopValue i))
                                            initTypes [0 ..])
            (iStrs, iDepss, iCtxss) = unzip3 $ map (compileExprOptimized 1)
                                                   initValues
            (nStrs, nDepss, nCtxss) = unzip3 $ map (compileExprOptimized 2)
                                                   nExprs
            (sStr, sDeps, sCtxs) = compileExprOptimized 2 sExpr
            deps = H.unions $ sDeps : (iDepss ++ nDepss)
            initialization = foldr (\(iStr, ty, valueNameS) str ->
                                        showString (ty ++ " ") . valueNameS .
                                        showString "=" . iStr .
                                        showString ";" . str
                                   )
                                   (showString "")
                                   (zip3 iStrs initTypes valueNameSs)
            update = foldr (\(valueNameS, nStr) str ->
                               valueNameS .  showString "=" .
                               nStr . showString ";" . str
                           )
                           (showString "")
                           (zip valueNameSs nStrs)
        in ( ActionInfo (\c -> initialization .
                               showString "for(" . iterTypeS . showString " " .
                               iterNameS .  showString "=0;" . iterNameS .
                               showString "<" . iterStr .
                               showString ";++" . iterNameS .
                               showString "){" . c .  showString "if(" . sStr .
                               showString "){break;}" . update .
                               showString "}")
                        (H.map (const ()) deps)
                        (ShallowContext . H.unions $ sCtxs : (iCtxss ++ nCtxss))
           , deps )

actionName :: ActionID -> Int -> String
actionName aID i = concat ["a", hashName aID ++ "_" ++ show i]

contextVarName :: ContextVarType -> ActionID -> String
contextVarName LoopIteration = ('l' :) . hashName
contextVarName (LoopValue i) = flip actionName i

hashName :: ActionID -> String
hashName = printf "%x"

uniformName :: Int -> String
uniformName = ('u' :) . show

varyingName :: Int -> String
varyingName = ('v' :) . show

attributeName :: Int -> String
attributeName = ('t' :) . show
