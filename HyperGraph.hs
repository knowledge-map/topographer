import Concepts

import qualified Data.Set as Set

data HyperGraph a = HyperGraph (Set.Set (Node a)) (Set.Set (HyperEdge a))
    deriving (Ord, Eq)
data Graph      a = Graph      (Set.Set (Node a)) (Set.Set (Edge a))
    deriving (Ord, Eq)

data HyperEdge  a = HyperEdge (Node a) (Set.Set (Node a))
    deriving (Ord, Eq)
data Edge       a = Edge      (Node a) (Node a)
    deriving (Ord, Eq)

data Node       a = Node a
    deriving (Ord, Eq)

instance Show a => Show (HyperGraph a) where
    show (HyperGraph ns hes) = "digraph G {\n"
        ++ (unlines $ map show $ Set.toList ns)
        ++ (unlines $ map show $ Set.toList hes)
        ++ "}"

instance Show a => Show (Graph a) where
    show (Graph ns es) = "digraph G {\n"
        ++ (unlines $ map show $ Set.toList ns)
        ++ (unlines $ map show $ Set.toList es)
        ++ "}"

instance Show a => Show (HyperEdge a) where
    show (HyperEdge n ns) = show n ++ " -> " ++ nodeSet ++ "\n"
        ++ nodeSet ++ " -> " ++ show n ++ "\n"
        -- ++ (unlines $ map (\n' -> nodeSet ++ " -> " ++ (show n')) $ Set.toList ns) ++ "\n" 
        ++ nodeSet ++ "[label=\"\"]"
        where nodeSet = concat $ map show $ Set.toList ns

instance Show a => Show (Edge a) where
    show (Edge n n') = (show n) ++ " -> " ++ (show n')

instance Show a => Show (Node a) where
    show (Node n) = map (\c -> if c == ' ' || c == '"' then '_' else c) $ show n


{-
    Takes a HyperGraph and returns a DAG in which the descendants
    of a Node have a superset of the HyperEdges of the Node.

    Conversely for each ancestor of a Node the set of HyperEdges to
    the ancestor is a subset of the set of HyperEdges to the Node.
-}
subsetDAG :: Ord a => HyperGraph a -> Graph a
subsetDAG h = Graph ns (Set.fromList es')
    where
        es' = [Edge n n' | n <- ns', n' <- ns', (inHyperEdges h n) `Set.isSubsetOf` (inHyperEdges h n')]
        ns' = Set.toList ns
        HyperGraph ns hes = h


inHyperEdges :: Ord a => HyperGraph a -> Node a -> Set.Set (HyperEdge a)
inHyperEdges h n = Set.filter (partOfHyperEdge n) hes
    where
        HyperGraph ns hes = h


partOfHyperEdge :: Ord a => Node a -> HyperEdge a -> Bool
partOfHyperEdge n he = n `Set.member` es
    where
        HyperEdge n' es = he


main :: IO ()
main = do
    --putStrLn "HyperGraph experiments"
    putStrLn $ show resourceHyperGraph
    --putStrLn $ show $ dag 
    --print conceptResources

    where
        dag = subsetDAG resourceHyperGraph

        resourceHyperGraph = HyperGraph (Set.fromList resourceNodes) (Set.fromList resourceEdges)
        resourceNodes = map Node $ resources
        resourceEdges = concat [ [HyperEdge (Node r) (Set.fromList $ map Node rs) | rs <- resourceResources r ] | r <- resources] :: [HyperEdge String]
        resourceResources r = [ find c conceptResources | c <- find r resourceConcepts ] :: [[String]]

        conceptHyperGraph = HyperGraph (Set.fromList conceptNodes) (Set.fromList conceptEdges)
        conceptNodes = map Node $ concepts
        conceptEdges = concat [ [HyperEdge (Node c) (Set.fromList $ map Node cs) | cs <- conceptConcepts c ] | c <- concepts] :: [HyperEdge String]
        conceptConcepts c = [ find r resourceConcepts | r <- find c conceptResources ] :: [[String]]

find :: String -> [(String, [String])] -> [String]
find n al = head [b | (a, b) <- al, n == a]
