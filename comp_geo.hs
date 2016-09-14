import Data.List
{- fish fish fish fish fish fish fish fish

ok so I need to solve the closest pair problem:
given n points in a metric space - find the two closest points

so i need to define a metric space:
axioms:
Given a set A and a function d:A x A->R (A,d) a metric space iff
forall x,y,z in A,
    1). d(x,y) = 0 <=> x = y 
    2). d(x,y) = d(y,x)
    3). d(x,z) <= d(x,y) + d(y,z)

and a distance function - so i could do this for a few different spaces: obv try euclidean first - then maybe hyperbolic, then use some fancy metrics from vector spaces? -}

-- 2d euclidean space
eucDist :: Floating r => (r,r) -> (r,r) -> r
eucDist (x1,x2) (y1,y2) = sqrt $ (x1-y1)**2 + (x2-y2)**2

--minimum of a list
minList = head . sort

--closest points in a set
eucClosest :: [(Double, Double)] -> (Double, ((Double, Double), (Double, Double)))
eucClosest xs = minList [ (eucDist x y,(x,y)) | x <- xs, y <- xs, x /= y]
