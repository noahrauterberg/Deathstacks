# Death Stacks

This implementation of the [`Deathstacks`](https://en.wikipedia.org/wiki/Death_Stacks) board game was made as part of "Softwaretechnik und Propraggmierparadigmen" at TU Berlin.
I implemented the game logic, which was made to work with a python game server that was provided by the university but is not part of the repository.

All implemented functions were tested thoroughly.

---

This Haskell implementation works with the following functions:

* `validateFEN :: String -> Bool` - checks whether a given String matches the expected FEN-Notation (similar to chess FEN)
* `buildBoard :: String -> Bool` - given a valid FEN string, it builds a new board of type `[[Cell]]`
* `path :: Pos -> Dir -> Int -> [Pos]` - returns an Int-long path from Pos in direction Dir
* `playerWon :: Board -> Maybe Player` - checks whether a player has won the given board
* `possibleMoves :: Pos -> Cell -> [Move]`- returns a list of all possible moves for a given position and stack
* `isValidMove :: Board -> Move -> Bool` - checks whether a given move is valid on the current board
* `listMoves :: Board -> Player -> [Move]` - lists all possible moves for a player on the current board
