module Area where  

data Point = Point Float Float
data Shape = Rectangle Point Float Float | Circle Point Float | Triangle Point Point Point Float

dist :: Point ->Point-> Float
dist (Point x y) (Point x2 y2) = sqrt((((x2 -x) * (x2 -x)) + ((y2-y) * (y2-y)))) 

area :: Shape -> Float
area (Rectangle p x y) = x * y
area (Circle p r) = 2 * 3.1416 * r
area (Triangle  x  y z h) = ((dist y z) * h) / 2.0