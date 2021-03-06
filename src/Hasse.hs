{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Hasse where

import           Protolude               hiding (head)

import           Data.Graph              hiding (Edge, edges)
import           Data.Graph.Automorphism
import           Data.List               (head, last, nub, nubBy)
import           Math.Core.Utils         (picks)

type Element = Int

transitiveClosure :: [(Element, Element)] -> [(Element, Element)]
transitiveClosure poset
  | poset == closure = poset
  | otherwise = transitiveClosure closure
  where closure = nub $ poset ++ [(a, c) | (a, b) <- poset, (b', c) <- poset, b == b']

connectElement :: [(Element, Element)] -> [Element] -> Element -> [[(Element, Element)]]
connectElement poset elementSet element = (:poset) <$> allowedEdges
  where edges = ((element,) <$> elementSet) <> ((,element) <$> elementSet)
        isIdentity (x, y) = x == y
        isTransitive e = hasTransitive (e : poset)
        isContradiction e = hasContradiction (e : poset)
        isAllowed = not <$> isIdentity <||> isTransitive <||> isContradiction
        allowedEdges = filter isAllowed edges
        (<||>) = liftA2 (||)

hasTransitive :: [(Element, Element)]-> Bool
hasTransitive xs = or [edge `elem` transitiveClosure rest | (edge, rest) <- picks xs]

hasContradiction :: [(Element, Element)]-> Bool
hasContradiction xs = or [swap edge `elem` transitiveClosure rest | (edge, rest) <- picks xs]

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
allPosets elementSet = concat $ generateUntilNull (pruneIsomorphisms elementSet . foldMap (addEdge elementSet)) [[]]

generateUntilNull :: ([a] -> [a]) -> [a] -> [[a]]
generateUntilNull f = takeWhile (not . null) . iterate f

rows :: [(Element, Element)] -> [Element] -> [[Element]]
rows poset elementSet = (\x -> filter (\y -> rowOfElement poset y == x) elementSet) <$> [0..height poset]

rowOfElement :: [(Element, Element)] -> Element -> Int
rowOfElement poset element =
  case lookupAll element poset of
    []    -> 0
    nexts -> 1 + maximum [rowOfElement poset next | next <- nexts]

lookupAll :: Eq a => a -> [(a, a)] -> [a]
lookupAll e assl = [y | (x, y) <- assl, x == e]

height :: [(Element, Element)] -> Int
height = length . longestChain

longestChain :: [(Element, Element)] -> [(Element, Element)]
longestChain poset = fromMaybe [] (headMay =<< lastMay chainSearch)
  where chainSearch = takeWhile (not . null) $ iterate (lengthenChains poset) [[edge] | edge <- poset]

lengthenChains :: [(Element, Element)] -> [[(Element, Element)]] -> [[(Element, Element)]]
lengthenChains poset = foldMap lengthenChain
  where lengthenChain chain =
          case headMay chain of
            Nothing       -> [[edge] | edge <- poset]
            Just (end, _) -> [next:chain | next <- poset, snd next == end]
