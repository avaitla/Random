module Pegasos where

import qualified Data.Vector as V

norm_2 :: V.Vector Double -> Double
norm_2 = sqrt . V.sum . V.map (^2)

dot_prod :: Num a => V.Vector a -> V.Vector a -> a
dot_prod vec1 vec2 = V.sum $ V.zipWith (*) vec1 vec2

subtract_vec :: Num a => V.Vector a -> V.Vector a -> V.Vector a
subtract_vec = V.zipWith (-)

radial_basis gamma vec1 vec2 | gamma > 0 = error "Gamma Must be Larger than 0"
							 | otherwise = -1 * gamma * (norm_2 sub_vect) ^ 2 where
	sub_vect = subtract_vec vec1 vec2
	
hom_poly degree vec1 vec2 | degree <= 0 = error "Polynomial Degree Must Be Less than 0"
						  | otherwise = (dot_prod vec1 vec2) ^ degree
						  
inhom_poly degree vec1 vec2 | degree <= 0 = error "Polynomial Degree Must Be Less than 0"
						    | otherwise = ((dot_prod vec1 vec2) + 1) ^ degree

linear :: Num a => V.Vector a -> V.Vector a -> a
linear = hom_poly 1

hyp_tan kappa c vec1 vec2 | kappa <= 0 || c >= 0 = error "Invalid Parameters: Kappa Must be > 0, C < 0"
						  | otherwise = tanh (kappa * dot + c) where
	dot = dot_prod vec1 vec2