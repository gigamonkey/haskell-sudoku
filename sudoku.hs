import Debug.Trace (trace)


import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Vector (Vector, indexed, fromList, toList, (//), (!?), (!))
import System.Environment
import System.IO

import qualified Data.Set as S
import qualified Data.Vector as V

type Square = Int
type Digit  = Char
type Givens = [(Square, Digit)]
type Board  = Vector (S.Set Digit)

-- Some useful sets of indices ---------------------------------------

digits  = "123456789"
squares = [0..80]

blankSquare = S.fromList digits
blankBoard  = V.replicate 81 blankSquare

rows      = [ [ r*9 + c | c <- [0..8] ] | r <- [0..8] ]
cols      = [ [ r*9 + c | r <- [0..8] ] | c <- [0..8] ]
boxes     = [ [ r*9 + c | r <- [rs..rs+2], c <- [cs..cs+2] ] | rs <- [0,3,6], cs <- [0,3,6] ]
all_units = rows ++ cols ++ boxes

units = [ [ u | u <- all_units, elem s u ] | s <- squares ]
peers = [ delete s (foldl union [] (units !! s)) | s <- squares ]

-- To and from textual representation --------------------------------

givens :: [Char] -> Givens
givens text = [ (i, d) | (i, d) <- (zip [0..] (fromText text)), d `elem` digits ]

givensBoard :: Givens -> Board
givensBoard gs = blankBoard // [ (i, S.singleton d) | (i, d) <- gs ]

board :: [Char] -> Maybe Board
board text = foldl setDigit (Just blankBoard) (givens text)

oneline :: Board -> [Char]
oneline = map unsquare . toList

square c = if c == '.' then Nothing else Just c
unsquare s = if S.size s == 1 then (S.toList s) !! 0 else '.'

fromText = filter (`elem` ('.':digits))


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

emptySquare b = (V.map fst $ V.filter (\(i, s) -> S.size s > 1) $ indexed b) !? 0

assign :: Board -> Square -> Digit -> Maybe Board
assign b s d = if inPeers b s d then Nothing else setDigit (Just b) (s, d)

inPeers b s d = any inPeer [ (b!p) | p <- (peers !! s) ]
    where inPeer p = S.size p == 1 && S.member d p


setDigit :: Maybe Board -> (Square, Digit) -> Maybe Board
setDigit b (s, d) = foldl eliminateDigit b otherDigits
    where otherDigits = [ (s, d') | d' <- S.toList ((fromJust b) ! s), d' /= d ]

eliminateDigit :: Maybe Board -> (Square, Digit) -> Maybe Board
eliminateDigit Nothing _ = Nothing
eliminateDigit (Just b) (s, d) =
    let b' = b // [(s, S.delete d (b ! s))]
    in Just b'

-- Main --------------------------------------------------------------

main = do
  args <- getArgs
  forM_ args $ \a -> do
         puzzle <- readFile a
         -- putStrLn $ show $ givens puzzle
         putStrLn $ oneline $ givensBoard $ givens puzzle
         putStrLn $ case solve (board puzzle) of
                      Nothing -> "No solution."
                      Just b -> oneline b
