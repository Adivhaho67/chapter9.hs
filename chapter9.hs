---------------------------------------------------------
-- HC9T1: Parametric Type Synonym
---------------------------------------------------------
type Entity a = (String, a) -- (Name, Address/Value)

---------------------------------------------------------
-- HC9T2: Parametric Data Type Box
---------------------------------------------------------
data Box a = Empty | Has a
    deriving (Show)

---------------------------------------------------------
-- HC9T3: Add a number to a Box
---------------------------------------------------------
addN :: Num a => a -> Box a -> Box a
addN n Empty    = Empty
addN n (Has x)  = Has (x + n)

---------------------------------------------------------
-- HC9T4: Extract value from a Box
---------------------------------------------------------
extract :: a -> Box a -> a
extract def Empty   = def
extract _ (Has x)   = x

---------------------------------------------------------
-- HC9T5: Parametric Shape with Record Syntax
---------------------------------------------------------
data Shape a = Circle { colorC :: a, radius :: Float }
             | Rectangle { colorR :: a, width :: Float, height :: Float }
             deriving (Show)

---------------------------------------------------------
-- HC9T6: Recursive Tweet Data Type
---------------------------------------------------------
data Tweet = Tweet {
    content :: String,
    likes :: Int,
    comments :: [Tweet]
} deriving (Show)

---------------------------------------------------------
-- HC9T7: Engagement Function for Tweets
---------------------------------------------------------
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

---------------------------------------------------------
-- HC9T8: Recursive Sequence Data Type
---------------------------------------------------------
data Sequence a = SeqNode a (Sequence a) | SeqEnd
    deriving (Show)

---------------------------------------------------------
-- HC9T9: Check element in Sequence
---------------------------------------------------------
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ SeqEnd = False
elemSeq x (SeqNode y rest)
    | x == y    = True
    | otherwise = elemSeq x rest

---------------------------------------------------------
-- HC9T10: Binary Search Tree Data Type
---------------------------------------------------------
data BST a = EmptyBST | Node a (BST a) (BST a)
    deriving (Show)

-- Example insert function for BST
bstInsert :: (Ord a) => a -> BST a -> BST a
bstInsert x EmptyBST = Node x EmptyBST EmptyBST
bstInsert x (Node val left right)
    | x < val  = Node val (bstInsert x left) right
    | x > val  = Node val left (bstInsert x right)
    | otherwise = Node val left right -- ignore duplicates

---------------------------------------------------------
-- MAIN FUNCTION (tests all tasks)
---------------------------------------------------------
main :: IO ()
main = do
    putStrLn "HC9T1: Entity"
    let e :: Entity String
        e = ("Alice", "123 Street")
    print e

    putStrLn "\nHC9T2 & HC9T3: Box and addN"
    let b1 = Has 10
        b2 = Empty
    print (addN 5 b1)
    print (addN 5 b2)

    putStrLn "\nHC9T4: Extract"
    print (extract 0 b1)
    print (extract 0 b2)

    putStrLn "\nHC9T5: Shape"
    let c = Circle "Red" 5
        r = Rectangle "Blue" 10 5
    print c
    print r

    putStrLn "\nHC9T6 & HC9T7: Tweet and Engagement"
    let t1 = Tweet "Hello" 5 []
        t2 = Tweet "Reply" 3 []
        tMain = Tweet "Main Tweet" 10 [t1,t2]
    print tMain
    print (engagement tMain)

    putStrLn "\nHC9T8 & HC9T9: Sequence and elemSeq"
    let seqEx = SeqNode 1 (SeqNode 2 (SeqNode 3 SeqEnd))
    print seqEx
    print (elemSeq 2 seqEx)
    print (elemSeq 5 seqEx)

    putStrLn "\nHC9T10: BST"
    let bst = foldr bstInsert EmptyBST [5,3,7,1,4,6,8]
    print bst
