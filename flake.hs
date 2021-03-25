import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

--this code iterates on binary string to compute a snowflake
--state of hex grid is encoded as [0,0,1,0,0..]
--string is mapped to grid like:
--
-- 18 19 20 21 ...
--   9 10 11 12 13 14 15 16 17
-- 0  1  2  3  4  5  6  7  8 
--
--where "0" is string !! 0
--hex grid is in pointy top configuration
--1 in string denotes black hex
--0 denotes white hex

--grid is 32x32
--replace all 32 in code to change
--model is impressively infeficcient


----------------------------DISPLAY OF STATE-------------------------------

--maps index to coordinates of center of corresponding unit hexagon in R**2
cords :: Int -> (Float, Float)
cords i  
        --hex grid with pointy top has rows of 2 types
        | even     = (1.72*(fromIntegral $ i `mod` 32),      1.5*(fromIntegral $ i `div` 32))
        | not even = (0.86+1.72*(fromIntegral $ i `mod` 32), 1.5*(fromIntegral $ i `div` 32))
                where even = (i `div` 32) `mod` 2 == 0 


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

--design of Gloss needs this
window :: Display
window = InWindow "Nice Window" (1000, 1000) (10, 10) 

--not used here but very useful
displayState :: [Int] -> IO ()
displayState s = do display window white $ pictures $ map cordsToHex $ map cords $ oneAt s

--used for rendering
picState :: [Int] -> Picture
picState s = pictures $ map cordsToHex $ map cords $ oneAt s


------------------------ITERATION ON STATE -------------------------------

--gets values from state in the neighborhood of point i and point i itself
nei :: ([Int], Int) -> [Int]
nei (li, i) 
        --enforces border conditions
        | i < (32)+1      = [0,0,0,0,0,0,0]
        | i > (32^2)-32-1 = [0,0,0,0,0,0,0]

        --hex grid with pointy top has rows of 2 types
        | even     = [li !! i] ++ [li !! j | j <- [i+1, i+32, i+(32-1), i-1, i-(32+1), i-32]] 
        | not even = [li !! i] ++ [li !! j | j <- [i+1, i+(32+1), i+32, i-1, i-32, i-(32-1)]]
                where even = (i `div`32) `mod` 2 == 0 


--defines change of state of 1 hexagon given its neighborhood
--[Int] is len 7 list: element zero is value at point
--other 6 elements are values of neighbors
--we make hexagon 1 iff it was 0 and had only single 1 nearby
rule :: [Int] -> Int
rule local
        | local !! 0  == 1 = 1
        | sum local   == 1 = 1
        | otherwise        = 0

--applies rule to all hexagons
automata :: [Int] -> [Int]
automata s = [rule $ nei (s, i) | i <- [0..length s - 1]]

--design is weird because Gloss requires such; see docs of "simulate"
update :: ViewPort -> Float -> [Int] -> [Int]
update _ t state 
        --stops if too big to fit
        | sum state < round (32^2/3) = (iterate automata state) !! (round t)  
        | otherwise = state


main :: IO ()
main = do
        --state of 1 hexagon near the center of 32x32 grid
        let initial = take (round $ 32^2/2+32/2)       [0,0..]
                                                    ++ [1]  ++ 
                      take (round $ 1+(32^2)/2-(32/2)) [0,0..]  
        --output
        simulate window white 1 initial picState update


