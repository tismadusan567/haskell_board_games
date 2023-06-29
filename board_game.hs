import Data.List (transpose)
import Text.Parsec
import Control.Monad (foldM)

data Rose a = Node a [Rose a] deriving (Show)

-- 1. Struktura podataka Rose tree (10p)

-- e) napisati funkciju foldRose koja izršava fold (levi ili desni) na svim čvorovima
-- stabla tipa Rose (na primer ako imamo stablo sa celim brojevima i prosledimo
-- funkciju (+) i akumulator 0 kao rezultat se vraća zbir svih čvorova)

foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node val []) = f acc val
foldRose f acc (Node val children) = foldl (foldRose f) (f acc val) children

-- a) size - vraća broj čvorova stabla
-- height - računa visinu stabla, odnosno najdužu putanju (broj grana) od korena do
-- lista

size :: Rose a -> Int
size = foldRose (\acc _ -> acc + 1) 0

height :: Rose a -> Int
height (Node _ []) = 0
height (Node _ children) = 1 + maximum (map height children)


-- b) leavesCount - vraća broj listova,
-- leaves - vraća listu koja sadrži vrednosti svih listova stabla

leaves :: Rose a -> [a]
leaves (Node val []) = [val]
leaves (Node _ children) = children >>= leaves

leavesCount :: Rose a -> Int
leavesCount = length . leaves

-- c) elemsOnDepth - vraća vrednosti svih elemenat na određenoj dubini

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node val _) = [val]
elemsOnDepth depth (Node _ children) = children >>= elemsOnDepth (depth - 1)

-- d) instancirati tipsku klasu Functor za tip podataka Rose

instance Functor Rose where
    fmap :: (a -> b) -> Rose a -> Rose b
    fmap f (Node val children) = Node (f val) $ map (fmap f) children

test = Node 1 [Node 2 [Node 3 []], Node 4 [], Node 5 [Node 6 [], Node 7 []], Node 8 [Node 9 [Node 10 []], Node 11 []]]

-- 2. Predstavljanje stanja u igri (10p)

data Player = P1 | P2 deriving (Show, Eq)

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

data BoardState a = BoardState Player [[a]] deriving (Show)

data GameMove = GameMove Player (Int, Int) deriving (Show, Eq)

-- GameStateOp

newtype GameStateOp s a = GameStateOp { runGameState :: BoardState s -> (a,BoardState s) }

instance Functor (GameStateOp s) where
    fmap :: (a -> b) -> GameStateOp s a -> GameStateOp s b
    fmap f (GameStateOp op) = GameStateOp $ \state -> let (a, newState) = op state in (f a, newState)

instance Applicative (GameStateOp s) where
    pure :: a -> GameStateOp s a
    pure x = GameStateOp $ \state -> (x, state)

    (<*>) :: GameStateOp s (a -> b) -> GameStateOp s a -> GameStateOp s b
    (GameStateOp op1) <*> (GameStateOp op2) = GameStateOp $ \state ->
        let (f, state1) = op1 state
            (res, state2) = op2 state1
        in (f res, state2)

instance Monad (GameStateOp s) where
    return :: a -> GameStateOp s a
    return = pure

    (>>=) :: GameStateOp s a -> (a -> GameStateOp s b) -> GameStateOp s b
    (GameStateOp op) >>= f = GameStateOp $ \state ->
        let (a, newState) = op state
            (GameStateOp newOp) = f a
        in newOp newState

-- GameStateOpHistory

newtype GameStateOpHistory s a = GameStateOpHistory { runGameStateH :: BoardState s -> (a,[BoardState s]) }
-- newtype GameStateOpHistory s a = GameStateOpHistory { runGameStateH :: [BoardState s] -> (a,[BoardState s]) }

instance Functor (GameStateOpHistory s) where
    fmap :: (a -> b) -> GameStateOpHistory s a -> GameStateOpHistory s b
    fmap f (GameStateOpHistory op) = GameStateOpHistory $ \states -> let (a, newStates) = op states in (f a, newStates)

instance Applicative (GameStateOpHistory s) where
    pure :: a -> GameStateOpHistory s a
    pure x = GameStateOpHistory $ \state -> (x, [state])

    (<*>) :: GameStateOpHistory s (a -> b) -> GameStateOpHistory s a -> GameStateOpHistory s b
    (GameStateOpHistory op1) <*> (GameStateOpHistory op2) = GameStateOpHistory $ \state ->
        let (f, states1) = op1 state
            (res, states2) = op2 (head states1)
        in (f res, states2 ++ states1)

instance Monad (GameStateOpHistory s) where
    return :: a -> GameStateOpHistory s a
    return = pure

    (>>=) :: GameStateOpHistory s a -> (a -> GameStateOpHistory s b) -> GameStateOpHistory s b
    (GameStateOpHistory op) >>= f = GameStateOpHistory $ \state ->
        let (res1, states1) = op state
            (GameStateOpHistory newOp) = f res1
            (res2, states2) = newOp (head states1)
        in (res2, states2 ++ states1)

-- 3. Primena kreiranih tipova na primeru igre Iks-Oks (12p)

data XOField = X | O | P deriving (Eq)

instance Show XOField where
    show X = "X"
    show O = "O"
    show P = " "

playerToXOField :: Player -> XOField
playerToXOField P1 = X
playerToXOField P2 = O

-- Napraviti ispis iks-oks table u sledećem formatu:

showXOState :: BoardState XOField -> String
showXOState (BoardState _ board) = board >>= showRow
    where showRow row = concatMap (("|"++) . show) row ++ "|\n"

-- instance Show (BoardState XOField) where
--     show state@(BoardState player board) = show player ++ "\n" ++ showXOState state

-- Napraviti funkciju koja vraća sve validne poteze u igri Iks-oks za neku datu tablu

validMoves :: BoardState XOField -> [GameMove]
validMoves state@(BoardState player board)
    | isFinished state = []
    | otherwise = [GameMove player (rowIndex, colIndex) |
    (rowIndex, row) <- zip [0..] board,
    (colIndex, el) <- zip [0..] row,
    el == P]

-- zatim funkciju koja vraća novo stanje table primenom jednog poteza

playMove :: BoardState XOField -> GameMove ->  BoardState XOField
playMove state@(BoardState player board) (GameMove movePlayer (newRowIndex, newColIndex))
    | movePlayer /= player = error "Mismatched move and state players"
    | isFinished state = state
    | otherwise = BoardState (nextPlayer player) newBoard
        where newBoard = [if rowIndex /= newRowIndex then row else transformRow row | (rowIndex, row) <- zip [0..] board]
              transformRow row = [if colIndex /= newColIndex then el else playerToXOField player | (colIndex, el) <- zip [0..] row]

-- Napisati funkciju koja proverava da li je tabla u završnom stanju, to znači da je ili
-- jedan od igrača povezao tri simbola ili da je tabla popunjena, a da niko nije pobedio.

isFinished :: BoardState XOField -> Bool
isFinished (BoardState _ board) =
    P `notElem` concat board
    || any checkSeq board
    || any checkSeq (transpose board)
    || checkSeq mainDiagonal
    || checkSeq secondaryDiagonal
    where checkSeq seq = all (X==) seq || all (O==) seq
          mainDiagonal = [el | (rowIndex, row) <- zip [0..] board, (colIndex, el) <- zip [0..] row, rowIndex == colIndex]
          secondaryDiagonal = [el | (rowIndex, row) <- zip [0..] board, (colIndex, el) <- zip [0..] row, rowIndex + colIndex + 1 == length row]

-- Korišćenje strukture podataka Rose (tačka 1) napraviti funkciju koja za proizvoljno
-- početno stanje table kreira stablo igre

createGameTree :: BoardState XOField -> Rose (BoardState XOField)
createGameTree state
    | isFinished state = Node state []
    | otherwise = Node state $ map (createGameTree . playMove state) (validMoves state)

testState = BoardState P1 [[P,P,P],[X,P,O],[P,X,P]]

iksOksInitialState = BoardState P1 [[P,P,P],[P,P,P],[P,P,P]]

-- 4. Implementacija promene stanja u igri iks-oks korišćenjem monada (12p)

createMove :: BoardState a -> (Int, Int) -> GameMove
createMove (BoardState player _) = GameMove player

applyMove :: (Int, Int) -> GameStateOp XOField Bool
applyMove pos = GameStateOp $ \state ->
    let newState = playMove state (createMove state pos) in (isFinished newState, newState)

applyMoveH :: (Int, Int) -> GameStateOpHistory XOField Bool
applyMoveH pos = GameStateOpHistory $ \state ->
    let newState = playMove state (createMove state pos) in (isFinished newState, [newState])

initialize = GameStateOpHistory $ const (False, [iksOksInitialState])

applyMoves :: GameStateOp XOField Bool
applyMoves = do
    applyMove (0,1)
    applyMove (1,0)
    applyMove (0,0)

applyMovesH :: GameStateOpHistory XOField Bool
applyMovesH = do
    initialize
    applyMoveH (0,1)
    applyMoveH (1,0)
    applyMoveH (0,0)

-- Parsiranje podataka o stanju na tabli i potezima u igri Iks-Oks (6p)

charToXOField :: Char -> XOField
charToXOField 'X' = X
charToXOField 'O' = O
charToXOField _ = P

charToPlayer :: Char -> Player
charToPlayer 'X' = P1
charToPlayer 'O' = P2
charToPlayer _ = P1

determinePlayer :: [[XOField]] -> Player
determinePlayer board
    | length (filter (==X) $ concat board) == length (filter (==O) $ concat board) = P1
    | otherwise = P2

parseField :: Parsec String () XOField
parseField = do
    c <- oneOf " XO"
    char '|'
    return (charToXOField c)

parseRow :: Parsec String () [XOField]
parseRow = do
    char '|'
    row <- count 3 parseField
    choice [endOfLine, eof >> return ' ']
    return row

parseMove :: Parsec String () GameMove
parseMove = do
    c <- oneOf " XO"
    space
    char '('
    row <- digit
    char ','
    col <- digit
    char ')'
    choice [endOfLine, eof >> return ' ']
    return $ GameMove (charToPlayer c) (read [row], read [col])

parseBoard :: Parsec String () (BoardState XOField, [GameMove])
parseBoard = do
    board <- count 3 parseRow
    moves <- many parseMove
    return (BoardState (determinePlayer board) board, moves)

main = do
    s <- readFile "board.txt"
    if null s
        then return ()
        else do case runParser parseBoard () "" s of
                        Right (state, moves) -> print $ stateAfterMoves state moves
                        Left err -> print err


-- Napisati funkciju koja vraća stanje na tabli koje se dobija posle izvršavanja svih
-- navedenih poteza redom. Ukoliko je neki od navedenih poteza nevalidan vratiti
-- odgovarajuću vrednost (na primer Nothing).

maybePlayMove :: Maybe (BoardState XOField) -> GameMove -> Maybe (BoardState XOField)
maybePlayMove Nothing move = Nothing
maybePlayMove (Just state) move
    | move `elem` validMoves state = Just (playMove state move)
    | otherwise = Nothing

stateAfterMoves :: BoardState XOField -> [GameMove] -> Maybe (BoardState XOField)
stateAfterMoves state = foldl maybePlayMove (Just state)

-- stateAfterMovesM :: BoardState XOField -> [GameMove] -> (Bool, BoardState XOField)
-- stateAfterMovesM state ((GameMove _ pos):moves) = 
--     runGameState (foldl (\acc (GameMove _ pos) -> acc >> applyMove pos) (applyMove pos) moves) state
    