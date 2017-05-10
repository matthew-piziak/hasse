{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

import           Protolude hiding (head)

import           Control.Applicative
import           Data.Foldable
import           Data.Graph              hiding (Edge)
import           Data.Graph.Automorphism
import           Data.List               (head, last, nub, nubBy)

type Element = Int

transitiveClosure :: [(Element, Element)] -> [(Element, Element)]
transitiveClosure poset
  | poset == closure = poset
  | otherwise = transitiveClosure closure
  where closure = nub $ poset ++ [(a, c) | (a, b) <- poset, (b', c) <- poset, b == b']

connectElement :: [(Element, Element)] -> [Element] -> Element -> [[(Element, Element)]]
connectElement poset elementSet element = (:poset) <$> allowedEdges
  where edges = ((element,) <$> elementSet) <> ((,element) <$> elementSet)
        tc = transitiveClosure poset
        isIdentity (x, y) = x == y
        isContradiction (x, y) = elem (y, x) tc
        isTransitive e = elem e tc
        isOvercoveringUp e = elem e $ fold [[(c, b), (b, c)] | (a, b) <- tc, (a', c) <- tc, a == a']
        isOvercoveringDn e = elem e $ fold [[(c, b), (b, c)] | (b, a) <- tc, (c, a') <- tc, a == a']
        isAllowed = (not <$>
                          isIdentity
                     <||> isContradiction
                     <||> isTransitive
                     <||> isOvercoveringUp
                     <||> isOvercoveringDn)
        allowedEdges = filter isAllowed edges
        (<||>) = liftA2 (||)

addEdge :: [Element] -> [(Element, Element)] -> [[(Element, Element)]]
addEdge elementSet poset = foldMap (connectElement poset elementSet) elementSet

pruneIsomorphisms :: [Element] -> [[(Element, Element)]] -> [[(Element, Element)]]
pruneIsomorphisms elementSet = nubBy (isIsomorphic' elementSet)

isIsomorphic' :: [Element] -> [(Element, Element)] -> [(Element, Element)] -> Bool
isIsomorphic' elementSet poset poset' = isIsomorphic graph graph'
  where bounds = (head elementSet, last elementSet)
        graph = buildG bounds poset
        graph' = buildG bounds poset'

allPosets :: [Element] -> [[(Element, Element)]]
allPosets = posetsDeepening [[]]

posetsDeepening :: [[(Element, Element)]] -> [Element] -> [[(Element, Element)]]
posetsDeepening soFar elementSet =
  concat $ generateUntilNull (\x -> pruneIsomorphisms elementSet $ foldMap (addEdge elementSet) x) [[]]

generateUntilNull :: ([a] -> [a]) -> [a] -> [[a]]
generateUntilNull f = takeWhile (not . null) . iterate f

height :: [(Element, Element)] -> Int
height = length . longestChain

longestChain :: [(Element, Element)] -> [(Element, Element)]
longestChain poset = fromMaybe [] (headMay =<< lastMay chainSearch)
  where chainSearch = takeWhile (not . null) $ iterate (lengthenChains poset) [[edge] | edge <- poset]

lengthenChains :: [(Element, Element)] -> [[(Element, Element)]] -> [[(Element, Element)]]
lengthenChains poset chains = foldMap lengthenChain chains
  where lengthenChain chain =
          case headMay chain of
            Nothing -> [[edge] | edge <- poset]
            Just (end, _) -> [next:chain | next <- poset, snd next == end]
