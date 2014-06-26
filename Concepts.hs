module Concepts where

data Concept a = Concept a
    deriving (Eq, Ord, Show)
data Resource a = Resource a
    deriving (Eq, Ord, Show)

data ResourceMap a = ResourceMap MapDescription (Resource a) (Concept a)
data MapDescription = Teaches | Requires | PartOf
    deriving (Eq)


teaches :: Resource a -> Concept a -> ResourceMap a
teaches = ResourceMap Teaches

requires :: Resource a -> Concept a -> ResourceMap a
requires = ResourceMap Requires

partOf :: Resource a -> Concept a -> ResourceMap a
partOf = ResourceMap PartOf


concepts = map Concept $
           [ "Mathematics"
           , "Counting"
           , "Addition"
           , "Subtraction"
           , "Multiplication"
           , "Division"
           , "The number line"
           , "Word problems"
           ]

resources = map Resource $
            [ "Counting with fingers"
            , "Counting to 100"
            , "Counting with the number line"
            , "Addition with fingers"
            , "Addition with the number line"
            , "Subtraction with fingers"
            , "Subtraction with the number line"
            , "Subtraction with the word problems"
            , "Multiplication with groups of addition"
            , "Division with groups of subtraction"
            , "Division practise"
            , "Division with word problems"
            , "Using money for beginners"
            , "Counting groups of money"
            , "Calculating differences"
            , "Maths Game I"
            , "Maths Game II"
            , "Maths Game III"
            ]

resourceMaps :: [ResourceMap String]
resourceMaps = concat $
    zipWith (each teaches) resources
    [ ["Counting"]
    , ["Counting"]
    , ["Counting", "The number line"]
    , ["Addition"]
    , ["Addition", "The number line"]
    , ["Subtraction"]
    , ["Subtraction"]
    , ["Subtraction"]
    , ["Multiplication"]
    , ["Division"]
    , ["Division"]
    , ["Division"]
    , []
    , ["Multiplication"]
    , []
    , []
    , []
    , []
    ]
    ++ zipWith (each requires) resources
    [ []
    , []
    , []
    , ["Counting"] 
    , ["Counting"]
    , ["Counting"]
    , ["Counting", "The number line"]
    , ["Counting", "Word problems"]
    , ["Counting", "Addition"] 
    , ["Counting", "Subtraction"]
    , []
    , ["Word problems"]
    , ["Counting", "Addition"]
    , ["Counting", "Addition"]
    , ["Subtraction", "Word problems"] 
    , ["Counting"]
    , ["Addition", "Subtraction"]
    , ["Multiplication", "Division"]
    ]
    ++ zipWith (each partOf) resources
    (repeat ["Mathematics"])

    where
        each f r cs = map (f r) (map Concept cs)

