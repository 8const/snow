import Graphics.Gloss

--this code will iterate on binary string to compute snowflakes
--currently it can display state but can't change it 
--state of hex grid is encoded as [1,0,1,0,0..]
--currently 8 hexagons in a row

--maps index to coordinates of center of corresponding unit hexagon in R**2
cords :: Int -> (Float, Float)
cords i  
        | even     = (1.72 * (fromIntegral $ i `mod` 8), 1.5 * (fromIntegral $ i `div` 8))
        | not even = (0.86 + 1.72 * (fromIntegral $ i `mod` 8), 1.5 * (fromIntegral $ i `div` 8))
                where even = (i `div` 8) `mod` 2 == 0 


--gets indices of 1s from binary string encoding state 
oneAt :: [Int] -> [Int]
oneAt list = filter (/= (-1)) [if list !! i == 1 then i else -1 | i <-[0..length list - 1]]

vertex :: Int -> (Float, Float)
vertex n = (cos $ fromIntegral n * pi/3.0 + pi/6.0, sin $ fromIntegral n * pi/3.0 + pi/6.0) 

--computes coordinates of all vertices of a unit hexagon at center 
hex :: [(Float, Float)]
hex = [vertex i | i <- [1..6]]

--moves hexagon and makes a picture of it
cordsToHex :: (Float, Float) -> Picture
cordsToHex (x, y) = translate x y $ polygon hex

--takes a picture of the state in [1,0,1,1..]
--not yet a function
pics = pictures $ map cordsToHex $ map cords $ oneAt [1,0,1,1,0,1,0,0,0,0,1,0,1,0,1,0,0,0]

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10) 

main :: IO ()
main = do
        display window white pics 
