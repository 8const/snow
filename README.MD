flake.hs computes and displays a snowflake by iterating on a binary string that represents a state of a 2 dimentional grid of hexagonal tiles. Implemented in Haskell. Gloss is used for displaying. 


If empty cell has 1 neighbor it turns on. 
A simple rule produces unexpectedly complex self-similiar shapes.
![alt text](https://github.com/8const/snow/blob/main/snow.jpg?raw=true)



Performance could be improved.
My computer isn't ok with 100x100 or more grid simulations.
