import Data.Set

data HyperGraph a = HyperGraph (Set (Node a)) (Set (HyperEdge a))
data Graph      a = Graph      (Set (Node a)) (Set (Edge a))

data HyperEdge  a = HyperEdge (Node a) (Set (Node a))
data Edge       a = Edge      (Node a) (Node a)

data Node       a = Node a


main :: IO ()
main = do
    print $ "HyperGraph experiments"
