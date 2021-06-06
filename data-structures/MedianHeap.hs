module MedianHeap where

import Data.Heap (Heap, HeapItem, MaxHeap, MinHeap, MinPolicy, size, viewHead, viewTail)
import qualified Data.Heap as Heap
import Data.List (partition, sort)
import Prelude hiding (head, null, tail)

data MedianHeap a = MedianHeap (MaxHeap a) (MinHeap a) deriving (Show)

head :: HeapItem pol item => Heap pol item -> item
head heap =
  case viewHead heap of
    Just x -> x
    Nothing -> error "empty heap"

tail :: HeapItem pol item => Heap pol item -> Heap pol item
tail heap =
  case viewTail heap of
    Just h -> h
    Nothing -> error "empty tail"

empty :: MedianHeap a
empty = MedianHeap Heap.empty Heap.empty

null :: MedianHeap a -> Bool
null (MedianHeap lesser _) = Heap.null lesser

median :: (Ord a, Fractional a) => MedianHeap a -> a
median (MedianHeap lesser greater)
  | size lesser > size greater = leftHead
  | size lesser < size greater = rightHead
  | otherwise = (leftHead + rightHead) / 2
  where
    leftHead = head lesser
    rightHead = head greater

insert :: (Ord a, Fractional a) => a -> MedianHeap a -> MedianHeap a
insert x heap@(MedianHeap lesser greater)
  | null heap = MedianHeap (Heap.singleton x) greater
  | x > median heap =
    balance $ MedianHeap lesser (Heap.insert x greater)
  | otherwise =
    balance $ MedianHeap (Heap.insert x lesser) greater

balance :: Ord a => MedianHeap a -> MedianHeap a
balance (MedianHeap lesser greater)
  | size lesser == size greater = MedianHeap lesser greater
  | size lesser > size greater =
    let (x, xs) = (head lesser, tail lesser)
     in MedianHeap xs (Heap.insert x greater)
  | otherwise =
    let (x, xs) = (head greater, tail greater)
     in MedianHeap (Heap.insert x lesser) xs

fromAscList :: Ord a => [a] -> MedianHeap a
fromAscList xs =
  let (lesserList, greaterList) = splitAt (length xs `div` 2) xs
   in MedianHeap (Heap.fromList lesserList) (Heap.fromList greaterList)

fromList :: Ord a => [a] -> MedianHeap a
fromList = fromAscList . sort

medianTwoLists :: (Ord a, Fractional a) => [a] -> [a] -> a
medianTwoLists xs ys =
  median $ foldr insert (fromAscList xs) ys

main = do
  print (medianTwoLists [5, 5, 7, 8] [1, 3])
