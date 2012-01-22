module LabelSet 
  ( LabelSet,
    newLabelSet,
    insertLabel,
    removeLabel,
    labelToInt,
    intToLabel,
    labelRange,
    intRange,
    labelsToInts,
    intsToLabels
  ) where

import Data.List
import Data.List.Utils
import Control.Monad

data LabelSet a = LabelSet { _labelToInt :: [(a, Int)], _nextInt :: Int }
	deriving Show
	
newLabelSet :: Eq a => [a] -> LabelSet a
newLabelSet a = LabelSet zipped len where
	zipped = zip new_list [0..(len - 1)]
	new_list = nub a
	len = length new_list

insertLabel :: Eq a => a -> LabelSet a -> LabelSet a
insertLabel a st@(LabelSet b d) = if (hasKeyAL a b) then st else result where
	result = LabelSet ((a, d) : b) (d + 1)

validLabel :: Eq a => a -> LabelSet a -> Bool
validLabel a = hasKeyAL a . _labelToInt

removeLabel :: Eq a => a -> LabelSet a -> LabelSet a
removeLabel a st@(LabelSet b d) = maybe st func (lookup a b) where
	func val = LabelSet (delFromAL b a) (d - 1) 

labelToInt :: Eq a => LabelSet a -> a -> Maybe Int
labelToInt a b = lookup b $ _labelToInt a

intToLabel :: Eq a => LabelSet a -> Int -> Maybe a
intToLabel (LabelSet a c) i = lookup i flipped where
	flipped = map (\(a,b) -> (b,a)) a
	
labelRange :: LabelSet a -> [a]
labelRange =  map fst . _labelToInt

intRange :: LabelSet a -> (Int, Int)
intRange a = (0, (_nextInt a - 1))

labelsToInts :: Eq a => [a] -> LabelSet a -> [Int]
labelsToInts a (LabelSet b c) = maybe (error "IllegalLabel") id (mapM (flip lookup b) a)

intsToLabels :: [Int] -> LabelSet a -> [a]
intsToLabels a (LabelSet b c) = maybe (error "IllegalInt") id (mapM (flip lookup flipped) a) 
	where flipped = map (\(a,b) -> (b,a)) b 