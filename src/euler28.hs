-- Were looking for the numbers of the corners of each square; numbering starts one under the top right corner at 1; 
-- their numbers are then (starting from right bottom, clockwise, h=height, w=width):
-- h, h + w-1, h + w-1 + h-1, h + w-1 + h-1 + w-1
-- Since w = h, this can be written even more simple
-- after that, h and w will increase with 2 (!), the next square will be like above, but with an extra term of the sum so far.
-- The starting point is 1 after which a next square is started. The endingpoint is when h or w >= 1001


listCorners lastTopRight w = let offset = w-1 
							 in
								 map (+lastTopRight) [4 * offset, 3 * offset, 2 * offset, offset]

sumCorners sumSoFar lastTopRight w = let corners = listCorners lastTopRight w
                                         topRight = head corners
                                         thisSum  = sumSoFar + sum corners 
			                         in  if w >= 1001 then thisSum else sumCorners thisSum topRight (w+2) 

main = print $ sumCorners 1 1 3 