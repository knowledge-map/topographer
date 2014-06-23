module Concepts where
concepts = [ "Mathematics"
           , "Counting"
           , "Addition"
           , "Subtraction"
           , "Multiplication"
           , "Division"
           , "The number line"
           , "Word problems"
           ]

resources = [ "Counting with fingers"
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

resourceConcepts = zip resources
    [ ["Mathematics", "Counting"]
    , ["Mathematics", "Counting"]
    , ["Mathematics", "Counting", "The number line"]
    , ["Mathematics", "Counting", "Addition"]
    , ["Mathematics", "Counting", "Addition", "The number line"]
    , ["Mathematics", "Counting", "Subtraction"]
    , ["Mathematics", "Counting", "Subtraction", "The number line"]
    , ["Mathematics", "Counting", "Subtraction", "Word problems"]
    , ["Mathematics", "Counting", "Addition", "Multiplication"]
    , ["Mathematics", "Counting", "Subtraction", "Division"]
    , ["Mathematics", "Division"]
    , ["Mathematics", "Division", "Word problems"]
    , ["Mathematics", "Counting", "Addition"]
    , ["Mathematics", "Counting", "Addition", "Multiplication"]
    , ["Mathematics", "Subtraction", "Word problems"]
    , ["Mathematics", "Counting"]
    , ["Mathematics", "Addition", "Subtraction"]
    , ["Mathematics", "Multiplication", "Division"]
    ] :: [(String, [String])]

conceptResources = (zip concepts $
    [ [ resource | (resource, cs) <- resourceConcepts, c `elem` cs ] | c <- concepts ]) :: [(String, [String])]

