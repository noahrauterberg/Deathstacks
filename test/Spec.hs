import Test.Hspec
import Board
import Deathstacks
-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################

testValidate :: Spec
testValidate = describe "Testing the validation of FEN strings" $ do
        it "test start position" $
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe`  True
        it "test random position" $
            validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` True
        it "test empty string" $
            validateFEN "" `shouldBe` False
        it "test empty position" $
            validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` True
        it "test only five rows with ending /" $
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/" `shouldBe` False
        it "test only five rows without ending /" $
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False
        it "test seven rows with ending /" $
            validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/" `shouldBe` False
        it "test seven rows without ending /" $
            validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False
        it "test seven coloums" $
            validateFEN ",,,,,,/,,,,,,/,,,,,,/,,,,,,/,,,,,,/,,,,,," `shouldBe` False
        it "test only five coloums" $
            validateFEN ",,,,/,,,,/,,,,/,,,,/,,,,/,,,," `shouldBe` False
        it "test wrong characters" $
            validateFEN "gg,,,,,gg/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` False

sampleBoard :: Board
sampleBoard = [[Stack [Red,Red], Stack [Red,Red], Stack [Red,Red], Stack [Red,Red], Stack [Red,Red], Stack [Red,Red]], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

secondBoard :: Board
secondBoard = [
    [Stack [Red, Red], Empty, Empty, Empty, Empty, Stack[Red, Red]], 
    [Empty, Empty, Empty, Empty, Empty, Empty], 
    [Empty, Stack[Blue, Blue, Red], Stack[Red, Red], Empty, Stack[Red, Red, Blue], Empty], 
    [Empty,Empty,Empty,Empty,Empty,Empty], 
    [Empty,Empty,Empty,Empty,Empty,Empty], 
    [Stack [Blue, Blue], Stack [Blue, Blue], Empty, Empty, Stack [Blue, Blue, Red, Red, Red, Blue], Stack [Blue, Blue]]
    ]

testBuildBoard :: Spec
testBuildBoard = describe "Testing board building" $ do
    it "test start position" $
        buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` sampleBoard
    it "test another board position" $
        buildBoard "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` secondBoard
testPath :: Spec
testPath = describe "Testing path creation" $ do
    -- normal movements on the board
    it "test North" $
        path (Pos 'a' 1) North 3 `shouldMatchList` [Pos 'a' 1, Pos 'a' 2, Pos 'a' 3, Pos 'a' 4]
    it "test NorthEast" $
        path (Pos 'a' 1) NorthEast 3 `shouldMatchList` [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3, Pos 'd' 4]
    it "test East" $
        path (Pos 'a' 1) East 3 `shouldMatchList` [Pos 'a' 1, Pos 'b' 1, Pos 'c' 1, Pos 'd' 1]
    it "test SouthEast" $
        path (Pos 'a' 6) SouthEast 3 `shouldMatchList` [Pos 'a' 6, Pos 'b' 5, Pos 'c' 4, Pos 'd' 3]
    it "test South" $
        path (Pos 'a' 6) South 3 `shouldMatchList` [Pos 'a' 6, Pos 'a' 5, Pos 'a' 4, Pos 'a' 3]
    it "test SouthWest" $
        path (Pos 'f' 6) SouthWest 3 `shouldMatchList` [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4, Pos 'c' 3]
    it "test West" $
        path (Pos 'f' 6) West 3 `shouldMatchList` [Pos 'f' 6, Pos 'e' 6, Pos 'd' 6, Pos 'c' 6]
    it "test NorthWest" $
        path (Pos 'f' 2) NorthWest 3 `shouldMatchList` [Pos 'f' 2, Pos 'e' 3, Pos 'd' 4, Pos 'c' 5]
    -- off-board movements for the four main directions
    it "test off-board North" $
        path (Pos 'c' 3) North 5 `shouldMatchList` [Pos 'c' 3, Pos 'c' 4, Pos 'c' 5, Pos 'c' 6, Pos 'c' 5, Pos 'c' 4]
    it "test off-board South" $
        path (Pos 'c' 3) South 4 `shouldMatchList` [Pos 'c' 3, Pos 'c' 2, Pos 'c' 1, Pos 'c' 2, Pos 'c' 3]
    it "test off-board East" $
        path (Pos 'c' 3) East 5 `shouldMatchList` [Pos 'c' 3, Pos 'd' 3, Pos 'e' 3, Pos 'f' 3, Pos 'e' 3, Pos 'd' 3]
    it "test off-board West" $
        path (Pos 'c' 3) West 4 `shouldMatchList` [Pos 'c' 3, Pos 'b' 3, Pos 'a' 3, Pos 'b' 3, Pos 'c' 3]
    -- off-board movements (mirrored in the corners)
    it "test mirror on corner a1" $
        path (Pos 'a' 1) SouthWest 3 `shouldMatchList` [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3, Pos 'd' 4]
    it "test mirror on corner a6" $
        path (Pos 'a' 6) NorthWest 3 `shouldMatchList` [Pos 'a' 6, Pos 'b' 5, Pos 'c' 4, Pos 'd' 3]
    it "test mirror on corner f6" $
        path (Pos 'f' 6) NorthEast 3 `shouldMatchList` [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4, Pos 'c' 3]
    it "test mirror on corner f1" $
        path (Pos 'f' 1) SouthEast 3 `shouldMatchList` [Pos 'f' 1, Pos 'e' 2, Pos 'd' 3, Pos 'c' 4]
    -- off-board movements (each test case combines two mirrored moves)
    it "test off-board NorthEast to South East" $
        path (Pos 'd' 5) NorthEast 4 `shouldMatchList` [Pos 'd' 5, Pos 'e' 6, Pos 'f' 5, Pos 'e' 4, Pos 'd' 3]
    it "test off-board SouthWest to NorthWest" $
        path (Pos 'c' 2) SouthWest 4 `shouldMatchList` [Pos 'c' 2, Pos 'b' 1, Pos 'a' 2, Pos 'b' 3, Pos 'c' 4]
    it "test off-board NorthWest to SouthEast" $
        path (Pos 'c' 5) NorthWest 4 `shouldMatchList` [Pos 'c' 5, Pos 'b' 6, Pos 'a' 5, Pos 'b' 4, Pos 'c' 3]
    it "test off-board SouthEast to NorthWest" $
        path (Pos 'd' 2) SouthEast 4 `shouldMatchList` [Pos 'd' 2, Pos 'e' 1, Pos 'f' 2, Pos 'e' 3, Pos 'd' 4]

-- more example boards
boardBlueWon :: Board
boardBlueWon = [[Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]

boardRedWon :: Board
boardRedWon = [
    [Stack [Red,Red,Blue,Blue],Stack [Red,Red,Blue,Blue],Stack [Red,Red,Blue,Blue],Stack [Red,Red,Blue,Blue],Stack [Red,Red,Blue,Blue],Stack [Red,Red,Blue,Blue]],
    [Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty]]

boardNoWin :: Board
boardNoWin = [
    [Stack [Red,Red],Empty,Empty,Empty,Empty,Stack [Red,Red]],
    [Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Stack [Red,Red],Empty,Stack [Red,Red,Blue],Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Stack [Blue,Blue,Red,Red,Red,Blue,Blue],Empty,Empty],
    [Stack [Blue],Stack [Blue,Blue],Empty,Empty,Empty,Stack [Blue,Blue]]]

testPlayerWon :: Spec
testPlayerWon = describe "Testing win detection" $ do
    it "test starting position" $
        playerWon sampleBoard `shouldBe` Nothing
    it "test secondBoard" $
        playerWon secondBoard `shouldBe` Nothing
    it "test Blue won" $
        playerWon boardBlueWon `shouldBe` Just Blue
    it "test Red won" $
        playerWon boardRedWon `shouldBe` Just Red
    it "test noWin" $
        playerWon boardNoWin `shouldBe` Nothing

movesCorner :: [Move]
movesCorner = [
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 3}, steps = 2},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 4}, steps = 3},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 5}, steps = 4},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 6}, steps = 5},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 5}, steps = 6},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'c', row = 3}, steps = 2},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'd', row = 4}, steps = 3},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 5}, steps = 4},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'f', row = 6}, steps = 5},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 5}, steps = 6},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'c', row = 1}, steps = 2},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'd', row = 1}, steps = 3},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 1}, steps = 4},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'f', row = 1}, steps = 5},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 1}, steps = 6}]

movesNoMin :: [Move]
movesNoMin = [
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'c', row = 4}, steps = 1},
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'd', row = 4}, steps = 1},
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'd', row = 3}, steps = 1},
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'd', row = 2}, steps = 1},
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'c', row = 2}, steps = 1},
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'b', row = 2}, steps = 1},
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'b', row = 3}, steps = 1},
    Move {start = Pos {col = 'c', row = 3}, target = Pos {col = 'b', row = 4}, steps = 1}]

validA1 :: [Move]
validA1 = [Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 2}, steps = 1},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 3}, steps = 2},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 4}, steps = 3},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 5}, steps = 4},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 6}, steps = 5},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 5}, steps = 6},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'b', row = 2}, steps = 1},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'c', row = 3}, steps = 2},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'd', row = 4}, steps = 3},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 5}, steps = 4},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'f', row = 6}, steps = 5},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 5}, steps = 6},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'b', row = 1}, steps = 1},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'c', row = 1}, steps = 2},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'd', row = 1}, steps = 3},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 1}, steps = 4},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'f', row = 1}, steps = 5},Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'e', row = 1}, steps = 6}]
testPossibleMoves :: Spec
testPossibleMoves = describe "Testing possibleMoves" $ do
    it "test empty cell" $
        possibleMoves (Pos 'c' 3) Empty `shouldMatchList` []
    it "test no minimum step" $
        possibleMoves  (Pos 'c' 3) (Stack [Red]) `shouldMatchList` movesNoMin
    it "test no-too-tall" $
        possibleMoves (Pos 'a' 1) (Stack [Red, Red, Red, Red, Red, Red]) `shouldMatchList` validA1

-- testValidMoves :: Spec
-- testValidMoves = describe "Testing validMoves" $ do
--     it "test corner position" $
--         validMoves (Pos 'a' 1) (Stack [Red, Red, Red, Red, Red, Red]) `shouldMatchList` movesCorner
--     it "test empty cell" $
--         validMoves (Pos 'c' 3) Empty `shouldBe` []
--     it "test no minimum step" $
--         validMoves  (Pos 'c' 3) (Stack [Red]) `shouldMatchList` movesNoMin

testIsValidMove :: Spec
testIsValidMove = describe "Testing valid moves" $ do
    it "test move is valid" $
        isValidMove sampleBoard (Move (Pos 'a' 1) (Pos 'a' 3) 2) `shouldBe` True
    it "test move is invalid no move" $
        isValidMove sampleBoard (Move (Pos 'a' 1) (Pos 'f' 6) 1) `shouldBe` False
    it "test not moving too tall stack" $
        isValidMove boardNoWin (Move (Pos 'a' 1) (Pos 'a' 2) 1) `shouldBe` False
    it "test move on empty cell" $
        isValidMove sampleBoard (Move (Pos 'c' 3) (Pos 'c' 4) 1) `shouldBe` False

allMovesStartBlue = [
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 2}, steps = 1},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'a', row = 3}, steps = 2},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'b', row = 2}, steps = 1},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'c', row = 3}, steps = 2},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'b', row = 1}, steps = 1},
    Move {start = Pos {col = 'a', row = 1}, target = Pos {col = 'c', row = 1}, steps = 2},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'b', row = 2}, steps = 1},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'b', row = 3}, steps = 2},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'c', row = 2}, steps = 1},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'd', row = 3}, steps = 2},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'c', row = 1}, steps = 1},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'd', row = 1}, steps = 2},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'a', row = 2}, steps = 1},
    Move {start = Pos {col = 'b', row = 1}, target = Pos {col = 'a', row = 1}, steps = 1},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'c', row = 2}, steps = 1},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'c', row = 3}, steps = 2},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'd', row = 2}, steps = 1},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'e', row = 3}, steps = 2},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'd', row = 1}, steps = 1},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'e', row = 1}, steps = 2},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'b', row = 2}, steps = 1},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'a', row = 3}, steps = 2},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'b', row = 1}, steps = 1},
    Move {start = Pos {col = 'c', row = 1}, target = Pos {col = 'a', row = 1}, steps = 2},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'd', row = 2}, steps = 1},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'd', row = 3}, steps = 2},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'e', row = 2}, steps = 1},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'f', row = 3}, steps = 2},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'e', row = 1}, steps = 1},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'f', row = 1}, steps = 2},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'c', row = 2}, steps = 1},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'b', row = 3}, steps = 2},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'c', row = 1}, steps = 1},
    Move {start = Pos {col = 'd', row = 1}, target = Pos {col = 'b', row = 1}, steps = 2},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'e', row = 2}, steps = 1},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'e', row = 3}, steps = 2},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'f', row = 2}, steps = 1},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'f', row = 1}, steps = 1},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'd', row = 2}, steps = 1},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'c', row = 3}, steps = 2},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'd', row = 1}, steps = 1},
    Move {start = Pos {col = 'e', row = 1}, target = Pos {col = 'c', row = 1}, steps = 2},
    Move {start = Pos {col = 'f', row = 1}, target = Pos {col = 'f', row = 2}, steps = 1},
    Move {start = Pos {col = 'f', row = 1}, target = Pos {col = 'f', row = 3}, steps = 2},
    Move {start = Pos {col = 'f', row = 1}, target = Pos {col = 'e', row = 2}, steps = 1},
    Move {start = Pos {col = 'f', row = 1}, target = Pos {col = 'd', row = 3}, steps = 2},
    Move {start = Pos {col = 'f', row = 1}, target = Pos {col = 'e', row = 1}, steps = 1},
    Move {start = Pos {col = 'f', row = 1}, target = Pos {col = 'd', row = 1}, steps = 2}]

testListMoves :: Spec
testListMoves = describe "Test list moves" $ do
    it "test starting position, blue" $
        listMoves sampleBoard Blue `shouldMatchList` allMovesStartBlue

testMove = Move {start = Pos {col = 'f', row = 1}, target = Pos {col = 'd', row = 1}, steps = 2}

testInstanceEq :: Spec
testInstanceEq = describe "Test Eq Instances" $ do
    it "equal players" $
        Red == Red `shouldBe` True
    it "non-equal players" $
        Red /= Blue `shouldBe` True
    it "equal cell" $
        Empty == Empty `shouldBe` True
    it "non-equal cell" $
        Empty /= Empty `shouldBe` False
    it "equal move" $
        testMove == testMove `shouldBe` True
    it "non-queal move" $
        testMove /= testMove `shouldBe` False

testMoveComponents :: Spec
testMoveComponents = describe "Test Move" $ do
    it "start position" $
        show (start testMove) `shouldBe` "Pos {col = 'f', row = 1}"
    it "target position" $
        show (target testMove) `shouldBe` "Pos {col = 'd', row = 1}"
    it "steps" $
        show (steps testMove) `shouldBe` "2"

testShowInstanceMove :: Spec
testShowInstanceMove = describe "Test Show Instance Move" $ do
    it "Move (Pos 'a' 1) (Pos 'a' 2) 1" $
        show (Move (Pos 'a' 1) (Pos 'a' 2) 1) `shouldBe` "a1-1-a2"

main :: IO ()
main = hspec $ do 
    testValidate
    testBuildBoard
    testPath
    testPlayerWon
    testPossibleMoves
    -- testValidMoves
    testIsValidMove
    testListMoves
    testInstanceEq
    testMoveComponents
    testShowInstanceMove
