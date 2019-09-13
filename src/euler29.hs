import qualified Data.Set as Set

-- list all powers a^b for 2 <= a,b <= 100. We need a type large enough to store 100 ^ 100 for this. 
-- put them in a set and return the length

powers :: [Integer]
powers = [ a ^ b | a <- [2..100], b <- [2..100]]

main = print $ Set.size $ Set.fromList powers