import Control.Applicative
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

rows  = [ [ r*9 + c | c <- [0..8] ] | r <- [0..8] ]
cols  = [ [ r*9 + c | r <- [0..8] ] | c <- [0..8] ]
boxes = [ [ r*9 + c | r <- [rs..rs+2], c <- [cs..cs+2] ] | rs <- [0,3,6], cs <- [0,3,6] ]

squares   = [0..80]
all_units = rows ++ cols ++ boxes
units     = [ [ u | u <- all_units, elem s u ] | s <- squares ]
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

-- The main function
solve :: Maybe Board -> Maybe Board
solve Nothing  = Nothing
solve (Just b) = search (Just b) (emptySquare b) digits

search :: Maybe Board -> Maybe Square -> [Digit] -> Maybe Board
search Nothing _ _              = Nothing -- dead end in assigment
search (Just _) (Just _)  []    = Nothing -- ran out of digits to try
search (Just b) Nothing _       = Just b  -- solved it!
search (Just b) (Just s) (d:ds) =
    case assign b s d of
      Nothing -> tryNextDigit
      next    -> nextStep next
    where tryNextDigit = search (Just b) (Just s) ds
          nextStep b'  = solve b' <|> tryNextDigit

-- Get an empty square to try filling.
emptySquare b = listToMaybe $ map fst $ filter (isNothing . snd) $ assocs b

-- Assign a given digit to the given square. Return Nothing if that
-- leads to an immediate contradiction.
assign b s d = if inPeers b s d then Nothing else Just (b // [(s, Just d)])

inPeers b s d = any inPeer (peers !! s)
    where inPeer p = maybe False (d==) (b ! p)

main = do
  args <- getArgs
  emit args
  where emit (a:as) = do
          puzzle <- readFile a
          putStrLn $ oneline (givens puzzle)
          putStrLn $ case solve (board puzzle) of
                       Nothing -> "No solution."
                       Just b -> oneline b
          emit as
        emit [] = do return ()
