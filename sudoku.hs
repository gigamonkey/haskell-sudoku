import Control.Applicative
import Control.Monad
import Data.Array -- switch to Data.Vector
import Data.List
import Data.Maybe
import System.Environment
import System.IO

type Square = Int
type Digit  = Char
type Board  = Array Square (Maybe Char)

digits  = "123456789"
squares = [0..80]

rows      = [ [ r*9 + c | c <- [0..8] ] | r <- [0..8] ]
cols      = [ [ r*9 + c | r <- [0..8] ] | c <- [0..8] ]
boxes     = [ [ r*9 + c | r <- [rs..rs+2], c <- [cs..cs+2] ] | rs <- [0,3,6], cs <- [0,3,6] ]
all_units = rows ++ cols ++ boxes

units = [ [ u | u <- all_units, elem s u ] | s <- squares ]
peers = [ delete s (foldl union [] (units !! s)) | s <- squares ]


-- To and from textual representation --------------------------------

givens :: [Char] -> Board
givens text = listArray (0, 80) (fromText text)

board :: [Char] -> Maybe Board
board text = Just (givens text)

oneline :: Board -> [Char]
oneline = map unsquare . elems

square c = if c == '.' then Nothing else Just c
unsquare = fromMaybe '.'
fromText = map square . filter (`elem` ('.':digits))


-- Solving code ------------------------------------------------------

solve :: Maybe Board -> Maybe Board
solve board = do
  b <- board
  case emptySquare b of
    Nothing -> board
    Just s  -> search b digits s

search :: Board -> [Digit] -> Square -> Maybe Board
search b digits s =
    case digits of
      d:ds ->
          case assign b s d of
            Nothing -> tryNextDigit
            next    -> solve next <|> tryNextDigit
          where tryNextDigit = search b ds s
      [] -> Nothing

emptySquare b = listToMaybe $ map fst $ filter (isNothing . snd) $ assocs b

assign b s d  = if inPeers b s d then Nothing else Just (b // [(s, Just d)])

inPeers b s d = any inPeer (peers !! s)
    where inPeer p = maybe False (d==) (b ! p)

-- Main --------------------------------------------------------------

main = do
  args <- getArgs
  forM_ args $ \a -> do
         puzzle <- readFile a
         putStrLn $ oneline (givens puzzle)
         putStrLn $ case solve (board puzzle) of
                      Nothing -> "No solution."
                      Just b -> oneline b
