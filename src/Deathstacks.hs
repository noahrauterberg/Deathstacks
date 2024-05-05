module Deathstacks where  -- do NOT CHANGE export of module

-- NOTE: this code was partially refactored using the vscode extension hlint, every use was commented

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board
import Data.List ( nub )
import Data.Char ( ord, chr)

-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 


-- #############################################################################
-- #################### playerWon :: Board -> Maybe Player  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

{- written using phind ai on gpt-4 model (see chat)
-- 
-- This function only detects whether there is only pne player who can move a stack or more
-- players is a list which contains the player who can move a stack (for each stack)
-- it then checks whether all entries in players are the same and 
-- returns Just Blue/Red if that is the case, else: Nothing
-}
playerWon :: Board -> Maybe Player
playerWon board =
    let players = [head stack | row <- board, Stack stack <- row]
    in if all (== head players) (tail players) then Just (head players) else Nothing

-- #############################################################################
-- #################### possibleMoves :: Pos -> Cell -> [Move]  ################
-- #################### - 4 Functional Points                   ################
-- #################### - 1 Coverage Point                      ################
-- #############################################################################

allDirections :: [Dir]
allDirections = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ Empty = []
possibleMoves pos (Stack stack) = let tupleList = nub (filter (\ (x,_) -> pos /= x) [(last (path pos dir i), i) | dir <- allDirections, i <- [1 .. (length stack)]])
  in [Move pos target steps| (target, steps) <- tupleList]


validMoves :: Pos -> Cell -> [Move]
validMoves _ Empty = []
validMoves pos (Stack stack) = let minimumSteps = if length stack > 4 then length stack - 4 else 1
  in let tupleList = nub (filter (\ (x,_) -> pos /= x) [(last (path pos dir i), i) | dir <- allDirections, i <- [minimumSteps .. (length stack)]])
  in [Move pos target steps| (target, steps) <- tupleList]

-- #############################################################################
-- #################### isValidMove :: Board -> Move -> Bool  ##################
-- #################### - 5 Functional Points                 ##################
-- #################### - 1 Coverage Point                    ##################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove board (Move (Pos startcol startrow) end steps) =
  let cell = (board!!(6 - startrow))!!(ord startcol - 97)
  in checkTooTall board cell
  && Move (Pos startcol startrow) end steps `elem` validMoves (Pos startcol startrow) cell

-- tooTall can only contain one stack, otherwise, the position is not reachable
checkTooTall :: Board -> Cell -> Bool
checkTooTall _ Empty = False
-- refactored using hlint
checkTooTall board (Stack (player:xs)) =
  let tooTallList = [stack | row <- board, Stack stack <- row, player == head stack, length stack > 4]
  in (null tooTallList || (let tooTall = Stack (head tooTallList)
  in tooTall == Stack (player:xs)))

-- #############################################################################
-- #################### listMoves :: Board -> Player -> [Move]  ################
-- #################### - 2 Functional Points                   ################
-- #################### - 1 Coverage Point                      ################
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves board player = let playerStacks = [Stack stack |row <- board, (Stack stack) <- row, head stack == player]
  in let playerPositions = [Pos colChar (6 - row) | row <- [0 .. 5], col <- [0 .. 5], board!!row!!col `elem` playerStacks, colChar <- ['a'..'f'], colChar == chr (col + 97)]
  in generateMoves playerPositions playerStacks

generateMoves :: [Pos] -> [Cell] -> [Move]
generateMoves [] [] = []
generateMoves (pos:ps) (cell:cs) = validMoves pos cell ++ generateMoves ps cs
