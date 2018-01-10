{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Graphics.Rendering.Ombra.Internal.SM (
        State(..),
        Route(..),
        Partial,
        job,
        route,
        routeLength
) where

import Control.Applicative
import qualified Data.List as L
import Data.Semigroup
import qualified Data.Map.Lazy as M
import Data.Sequence as S

class (Eq s, Ord s, Num (Cost s), Ord (Cost s)) => State s where
        type Cost s
        -- TODO: Diff? con invert :: Diff s -> Diff s
        cost :: s -> s -> Cost s
        initial :: s
        symmetric :: s -> Bool
        short :: (s, s) -> Cost s -> Int -> Bool

data Partial s = Partial { routes :: (M.Map s (Route s), M.Map s (Route s))
                         , states :: M.Map s ()
                         , savings :: M.Map (Cost s) [(s, s)]
                         }

data Route s = Route !(Seq s) (Seq s)
        deriving (Eq, Show)

instance State s => Semigroup (Partial s) where
        p <> p' = let newStates = M.union (states p) (states p')
                      len = M.size newStates
                      disjoint = len == M.size (states p) + M.size (states p')
                      (sl, sl') = (M.keys $ states p, M.keys $ states p')
                      (newSavings, preMerges) = calcSavings len sl sl'
                      allSavings = M.unionsWith (++) [ savings p
                                                     , savings p'
                                                     , M.fromListWith (++)
                                                                      newSavings
                                                     ]
                      allRoutes = let (f, l) | disjoint = routes p
                                             -- XXX
                                             | otherwise = sinRoutes (states p)
                                                                     (states p')
                                      (f', l') = routes p'
                                      allUnmerged = (M.union f f', M.union l l')
                                  in foldr merge allUnmerged preMerges
                  in Partial allRoutes newStates allSavings
                where sinRoutes s s' = let sd = M.difference s s'
                                           m = M.mapWithKey sinRoute sd
                                       in (m, m)
                      sinRoute s _ = Route (S.singleton s) (S.singleton s)

instance State s => Monoid (Partial s) where
        mempty = Partial (M.empty, M.empty) M.empty M.empty
        mappend = (<>)

invert :: Route s -> Route s
invert (Route seq rseq) = Route rseq seq

link :: Route s -> Route s -> Route s
link (Route seq rseq) (Route seq' rseq') = Route (seq >< seq') (rseq' >< rseq)

-- TODO: usare un'unica mappa con una tupla, possibile solo se è garantito che
-- se c'è un route che inizia con s allora ce ne è uno che finisce con s
merge :: State s
      => (s, s)
      -> (M.Map s (Route s), M.Map s (Route s))
      -> (M.Map s (Route s), M.Map s (Route s))
merge (i, j) (firstMap, lastMap) =
        case if symmetric i then searchAll else searchDirect of
             Just ((iFirst, ri), (jFirst, rj)) ->
                let r' = link (invertWhen iFirst ri)
                              (invertWhen (not jFirst) rj)
                    (fi, li) = (firstState ri, lastState ri)
                    (fj, lj) = (firstState rj, lastState rj)
                    (f', l') = (firstState r', lastState r')
                    same | iFirst == jFirst = False
                         | iFirst == True = fj == i
                         | jFirst == True = fi == j
                in if same
                   then (firstMap, lastMap)
                   else ( M.insert f' r' . M.delete fi . M.delete fj $ firstMap
                        , M.insert l' r' . M.delete li . M.delete lj $ lastMap
                        )
             Nothing -> (firstMap, lastMap)
    where searchIFirst = (,) True <$> M.lookup i firstMap
          searchJFirst = (,) True <$> M.lookup j firstMap
          searchILast = (,) False <$> M.lookup i lastMap
          searchJLast = (,) False <$> M.lookup j lastMap
          searchDirect = (,) <$> searchILast <*> searchJFirst 
          searchAll =     (,) <$> searchILast <*> searchJFirst
                      <|> (,) <$> searchIFirst <*> searchJFirst
                      <|> (,) <$> searchILast <*> searchJLast
                      <|> (,) <$> searchIFirst <*> searchJLast
          invertWhen b r = if b then invert r else r
          firstState (Route (x :<| _) _) = x
          lastState (Route (_ :|> x) _) = x

calcSavings :: State s => Int -> [s] -> [s] -> ([(Cost s, [(s, s)])], [(s, s)])
calcSavings n xs ys = foldr (\(s, s') (savs, pm) ->
                                let css' = cost s s'
                                    cs0 = cost s initial
                                    c0s' = cost initial s
                                    sav = (cs0 + c0s' - css', [(s, s')])
                                in if short (s, s') css' n
                                   then (savs, (s, s') : pm)
                                   else (sav : savs, pm))
                            ([], [])
                            (if symmetric (head xs)
                             then [(x, y) | x <- xs, y <- ys, x /= y]
                             else [f (x, y) | x <- xs, y <- ys, x /= y
                                            , f <- [id, \(x, y) -> (y, x)]])
job :: State s => s -> Partial s
job s | s /= initial = Partial (M.singleton s r, M.singleton s r)
                               (M.singleton s ())
                               M.empty
      | otherwise = mempty
        where r = Route (singleton s) (singleton s)

route :: State s => Partial s -> Route s
route p = iter (routes p) savingsList
        where iter routes@(m, _) (s : ss)
                | M.size m == 1 = head $ M.elems m
                | otherwise = iter (merge s routes) ss
              iter routes@(m, _) [] = foldr1 link (M.elems m)
              savingsList = concatMap snd . M.toDescList $ savings p

routeLength :: State s => Route s -> Cost s
routeLength (Route s _) = fst $ foldl (\(c, l) x -> (c + cost x l, x))
                                      (0, initial) s
