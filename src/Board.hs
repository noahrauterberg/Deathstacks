module Board where  -- do NOT CHANGE export of module

-- NOTE: this code was partially refactored using the vscode extension hlint, every use was commented

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.List.Split (splitOn)
import Data.Char (ord, chr)

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- #################  - 2 Functional Points                  ###################
-- #################  - 1 Coverage Point                     ###################
-- #############################################################################

-- Requirements:
-- 6 rows
-- 6 columns
-- allowed characters: 
-- infinite: 'a', 'b'
-- 5: ',', '/'
-- don't check: 24 pieces
-- don't check: maximum of 2 stacks with height > 4
validateFEN :: String -> Bool
validateFEN x = validateHelp x 0 0

validateHelp :: String -> Int -> Int -> Bool
validateHelp "" reihe spalte = reihe == 5 && spalte == 5
validateHelp (x:xs) reihe spalte = case x of
  'r' -> validateHelp xs reihe spalte
  'b' -> validateHelp xs reihe spalte
  ',' -> validateHelp xs reihe (spalte + 1)
  '/' -> (spalte == 5) && validateHelp xs (reihe + 1) 0
  _ -> False

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard x = buildHelp (splitOn "/" x)

-- refactored using hlint
buildHelp :: [String] -> Board
buildHelp = map (buildRow . splitOn ",")

buildRow :: [String] -> [Cell]
buildRow [] = []
buildRow ("":xs) = Empty : buildRow xs
buildRow (x:xs) = Stack (buildStack x): buildRow xs

buildStack :: String -> [Player]
buildStack "" = []
buildStack (x:xs) = case x of
  'r' -> Red : buildStack xs
  'b' -> Blue : buildStack xs

-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

-- path recursively calls calcNewPos (with mirrored direction if neccessary)
path :: Pos -> Dir -> Int -> [Pos]
path _ _ (-1) = []
path pos dir x = let (s1, newDir) = calcNewPos pos dir in pos : path s1 newDir (x-1)

-- checks if the pos needs to be mirrored
calcNewPos :: Pos -> Dir -> (Pos, Dir)
calcNewPos pos dir = let posNoMirror = newPos pos dir in
  -- if the move leads to off board movement, it has to be mirrored accordingly
  if row posNoMirror > 6 || row posNoMirror < 1 || col posNoMirror < 'a' || col posNoMirror > 'f' then
    -- yes, this is calculated again
    mirrorMove pos dir
  else
    (posNoMirror, dir)

mirrorMove :: Pos -> Dir -> (Pos, Dir)
-- these are all mirrored the same way in every case that they have to be mirrored
mirrorMove pos North = (newPos pos South, South)
mirrorMove pos South = (newPos pos North, North)
mirrorMove pos East = (newPos pos West, West)
mirrorMove pos West = (newPos pos East, East)
-- with these, it depends on what the position is
mirrorMove (Pos 'f' 6) NorthEast = (newPos (Pos 'f' 6) SouthWest, SouthWest)
mirrorMove (Pos 'f' row) NorthEast = (newPos (Pos 'f' row) NorthWest, NorthWest)
mirrorMove (Pos col 6) NorthEast = (newPos (Pos col 6) SouthEast, SouthEast)

mirrorMove (Pos 'f' 1) SouthEast = (newPos (Pos 'f' 1) NorthWest, NorthWest)
mirrorMove (Pos 'f' row) SouthEast = (newPos (Pos 'f' row) SouthWest, SouthWest)
mirrorMove (Pos col 1) SouthEast = (newPos (Pos col 1) NorthEast, NorthEast)

mirrorMove (Pos 'a' 1) SouthWest = (newPos (Pos 'a' 1) NorthEast, NorthEast)
mirrorMove (Pos 'a' row) SouthWest = (newPos (Pos 'a' row) SouthEast, SouthEast)
mirrorMove (Pos col 1) SouthWest = (newPos (Pos col 1) NorthWest, NorthWest)

mirrorMove (Pos 'a' 6) NorthWest = (newPos (Pos 'a' 6) SouthEast, SouthEast)
mirrorMove (Pos 'a' row) NorthWest = (newPos (Pos 'a' row) NorthEast, NorthEast)
mirrorMove (Pos col 6) NorthWest = (newPos (Pos col 6) SouthWest, SouthWest)

-- calculate the new position according to the direction
newPos :: Pos -> Dir -> Pos
newPos pos North = Pos (col pos) (row pos + 1)
newPos pos South = Pos (col pos) (row pos - 1)
newPos pos NorthEast = Pos (moveCol (col pos) East) (row pos + 1)
newPos pos East = Pos (moveCol (col pos) East) (row pos)
newPos pos SouthEast = Pos (moveCol (col pos) East) (row pos - 1)
newPos pos SouthWest = Pos (moveCol (col pos) West) (row pos - 1)
newPos pos West = Pos (moveCol (col pos) West) (row pos)
newPos pos NorthWest = Pos (moveCol (col pos) West) (row pos + 1)

moveCol :: Char -> Dir -> Char
moveCol x East = chr (ord x + 1)
moveCol x West = chr (ord x - 1)
