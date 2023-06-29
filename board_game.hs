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

data Player = First | Second

data BoardState a = BoardState Player [[a]]

data GameMove = GameMove Player (Int, Int)

newtype GameStateOp s a = GameStateOp { runState :: BoardState s -> (a,BoardState s) }

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

