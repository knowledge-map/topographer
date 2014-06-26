import Concepts

import qualified Data.Set as Set

data Graph a = Graph (Set.Set (Node a)) (Set.Set (Edge a a))
data Edge a b = Edge (Node a) (Node b)
    deriving (Eq, Ord)
data Node a = Node a
    deriving (Eq, Ord)

data BipartiteGraph a b = BipartiteGraph
    (Set.Set (Node a))   -- Node set a
    (Set.Set (Node b))   -- Node set b
    (Set.Set (Edge a b)) -- Edges from a to b
    (Set.Set (Edge b a)) -- Edges from b to a


instance Show a => Show (Graph a) where
    show (Graph ns es) = "digraph G {\n"
        ++ (unlines $ map show $ Set.toList ns)
        ++ (unlines $ map show $ Set.toList es)
        ++ "}"

instance (Show a, Show b) => Show (Edge a b) where
    show (Edge n n') = (show n) ++ " -> " ++ (show n')

instance Show a => Show (Node a) where
    show (Node n) = map (\c -> if c == ' ' || c == '"' then '_' else c) $ show n

instance (Show a, Show b) => Show (BipartiteGraph a b) where
   show (BipartiteGraph ns ns' es es') =  "digraph G {\n"
        ++ "{ node [shape=box]\n"
            ++ (unlines $ map show $ Set.toList ns)
        ++ "}\n\n"
        
        ++ "{ node [shape=ellipse]\n"
            ++ (unlines $ map show $ Set.toList ns')
        ++ "}\n\n"

        ++ (unlines $ map show $ Set.toList es)
        ++ "\n"
        ++ (unlines $ map show $ Set.toList es')
        ++ "}"


main :: IO ()
main = do
    print graph

    where
        graph = BipartiteGraph
            (Set.fromList resourceNodes) (Set.fromList conceptNodes)
            (Set.fromList resourceEdges) (Set.fromList conceptEdges)
        resourceNodes = map Node resources
        conceptNodes = map Node concepts
        resourceEdges = map (\(ResourceMap _ r c) -> Edge (Node r) (Node c))
                        $ filter (\(ResourceMap d _ _) -> d == Teaches) resourceMaps
        conceptEdges = map (\(ResourceMap _ r c) -> Edge (Node c) (Node r))
                        $ filter (\(ResourceMap d _ _) -> d == Requires) resourceMaps
