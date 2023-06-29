import Data.List (transpose)
data Rose a = Node a [Rose a]

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
size = foldRose (\acc el -> acc + 1) 0

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
    fmap f (Node val children) = Node (f val) $ map (fmap f) children

test = Node 1 [Node 2 [Node 3 []], Node 4 [], Node 5 [Node 6 [], Node 7 []], Node 8 [Node 9 [Node 10 []], Node 11 []]]

-- 2. Predstavljanje stanja u igri (10p)

data Player = First | Second deriving (Show)

data BoardState a = BoardState Player [[a]]

data GameMove = GameMove Player (Int, Int) deriving (Show)

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

newtype GameStateOpHistory s a = GameStateOpHistory { runGameStateH :: [BoardState s] -> (a,[BoardState s]) }

instance Functor (GameStateOpHistory s) where
    fmap :: (a -> b) -> GameStateOpHistory s a -> GameStateOpHistory s b
    fmap f (GameStateOpHistory op) = GameStateOpHistory $ \states -> let (a, newStates) = op states in (f a, newStates)

instance Applicative (GameStateOpHistory s) where
    pure :: a -> GameStateOpHistory s a
    pure x = GameStateOpHistory $ \states -> (x, states)

    (<*>) :: GameStateOpHistory s (a -> b) -> GameStateOpHistory s a -> GameStateOpHistory s b
    (GameStateOpHistory op1) <*> (GameStateOpHistory op2) = GameStateOpHistory $ \states ->
        let (f, states1) = op1 states
            (res, states2) = op2 states1
        in (f res, states2)

instance Monad (GameStateOpHistory s) where
    return :: a -> GameStateOpHistory s a
    return = pure

    (>>=) :: GameStateOpHistory s a -> (a -> GameStateOpHistory s b) -> GameStateOpHistory s b
    (GameStateOpHistory op) >>= f = GameStateOpHistory $ \states ->
        let (a, newStates) = op states
            (GameStateOpHistory newOp) = f a
        in newOp newStates

-- 3. Primena kreiranih tipova na primeru igre Iks-Oks (12p)

data XOPolje = X | O | P deriving (Eq)

validMoves :: BoardState XOPolje -> [GameMove]
validMoves (BoardState player board) = [GameMove player (rowIndex, colIndex) |
    (rowIndex, row) <- zip [0..] board,
    (colIndex, el) <- zip [0..] row,
    el == P]

isFinished :: BoardState XOPolje -> Bool
isFinished (BoardState _ board) = 
    P `notElem` concat board 
    || any checkSeq board
    || any checkSeq (transpose board)
    || checkSeq [el | (rowIndex, row) <- zip [0..] board, (colIndex, el) <- zip [0..] row, rowIndex == colIndex]
    || checkSeq [el | (rowIndex, row) <- zip [0..] board, (colIndex, el) <- zip [0..] row, rowIndex + colIndex + 1 == length row]
    where checkSeq seq = all (X==) seq || all (O==) seq

testState = BoardState First [[P,P,P],[X,P,O],[P,X,P]]