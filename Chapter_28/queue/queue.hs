-- stack build criterion
-- stack ghc -- -O2 queue.hs
-- ./queue
import Criterion.Main
import Data.Sequence

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a = Queue { enqueue :: [a], dequeue :: [a] } deriving (Eq, Show)
-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue en de) = Queue (x:en) de

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue en []) = pop (Queue [] (Prelude.reverse en))
pop (Queue en (x:xs)) = Just (x, Queue en xs)

pushList :: a -> [a] -> [a]
pushList = (:)

popList :: [a] -> Maybe (a, [a])
popList [] = Nothing
popList xs = Just (head xs', Prelude.reverse (tail xs'))
  where xs' = Prelude.reverse xs

pushSeq :: a -> Seq a -> Seq a
pushSeq = (<|)

popSeq :: Seq a -> Maybe (a, Seq a)
popSeq Empty = Nothing
popSeq xs = Just (x, Data.Sequence.reverse xs')
  where (x :<| xs') = Data.Sequence.reverse xs

myQueue = Queue [1..100] []
myList = [1..100]
mySeq = fromList [1..100]

altQueue :: Queue Int -> Queue Int
altQueue q = endq
  where midq = push 0 q
        Just (_, endq) = pop midq

altList :: [Int] -> [Int]
altList l = endl
  where midl = pushList 0 l
        Just (_, endl) = popList midl

altSeq :: Seq Int -> Seq Int
altSeq s = ends
  where mids = pushSeq 0 s
        Just (_, ends) = popSeq mids

testQueue :: Queue Int -> Int -> Queue Int
testQueue q 0 = q
testQueue q n = testQueue (altQueue q) (n-1)

testList :: [Int] -> Int -> [Int]
testList l 0 = l
testList l n = testList (altList l) (n-1)

testSeq :: Seq Int -> Int -> Seq Int
testSeq s 0 = s
testSeq s n = testSeq (altSeq s) (n-1)

main :: IO ()
main = defaultMain
    [ bench "testQueue" $
      whnf (testQueue myQueue) 1000
    , bench "testList" $
      whnf (testList myList) 1000
    , bench "testSeq" $
      whnf (testSeq mySeq) 1000
    ]
