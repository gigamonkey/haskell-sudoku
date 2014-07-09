import Data.Array
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import qualified Data.Set as Set

type Square = Int
type Digit  = Char
type Board  = Array Square (Maybe Char)

digits = "123456789"

emptyBoard :: Board
emptyBoard = listArray (0, 80) [ Nothing | _ <- [0..80] ]

sq r c = r*9 + c

rows  = [ [ sq r c | c <- [0..8] ] | r <- [0..8] ]
cols  = [ [ sq r c | r <- [0..8] ] | c <- [0..8] ]
boxes = [ [ sq r c | r <- [rs..rs+2], c <- [cs..cs+2] ] | rs <- [0,3,6], cs <- [0,3,6] ]

squares   = [0..80]
all_units = rows ++ cols ++ boxes
units     = [ [ u | u <- all_units, s `elem` u ] | s <- squares ]
peers     = [ delete s (foldl union [] (units !! s)) | s <- squares ]

-- To and from textual representation

square '.' = Nothing
square d   = Just(d)

unsquare Nothing = '.'
unsquare (Just d) = d

fromText = map square . filter (`elem` ('.':digits))

givens :: [Char] -> Board
givens text = listArray (0, 80) (fromText text)

board :: [Char] -> Maybe Board
board text = Just (givens text)

oneline = map unsquare . elems

-- Solving code

isEmpty (_, Nothing) = True
isEmpty (_, (Just _)) = False

emptySquare b =
    case map fst $ filter isEmpty $ assocs b of
      x:xs -> Just x
      []   -> Nothing


-- The main function
solve :: Maybe Board -> Maybe Board
solve Nothing = Nothing
solve (Just b) = search (Just b) (emptySquare b) digits

-- The meat of it.
search :: Maybe Board -> Maybe Square -> [Char] -> Maybe Board

-- We've already hit a dead end
search Nothing _ _ = Nothing

-- We've run out of digits for the current square.
search (Just _) (Just _)  [] = Nothing

-- No more empty squares, we're done!
search (Just b) Nothing _ = Just b

-- Still work to do on the current square.
search (Just b) (Just s) (d:ds) =
    case assign b s d of
      Nothing -> search (Just b) (Just s) ds
      Just b' -> case solve (Just b') of
                   Nothing -> search (Just b) (Just s) ds
                   solution -> solution

-- Assign the given digit to the given square and return either Just
-- the new board or Nothing if the assignment leads to a contradiction.
assign :: Board -> Square -> Digit -> Maybe Board

setDigit :: Board -> (Square, Digit) -> Board
setDigit b (s, d) = b // [(s, Just d)]

assign b s d =
    if inPeers b s d then Nothing else Just (setDigit b (s, d))

inPeer :: Board -> Square -> Digit -> Square -> Bool
inPeer b s d p =
    case b ! p of
      Nothing -> False
      Just d' -> d == d'

inPeers b s d = any (inPeer b s d) (peers !! s)

main = do
  args <- getArgs
  -- putStrLn $ show (units !! 0)
  -- putStrLn $ show (peers !! 0)
  emit args
  where emit (a:as) = do
          puzzle <- readFile a
          putStrLn $ oneline (givens puzzle)
          putStrLn $ case solve (board puzzle) of
                       Nothing -> "No solution."
                       Just b -> oneline b
          emit as
        emit [] = do return ()
