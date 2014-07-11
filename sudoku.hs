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
    where fromText = filter (`elem` ('.':digits))

board :: [Char] -> Maybe Board
board text = foldl set (Just blankBoard) (givens text)

justGivens :: [Char] -> Board
justGivens text = blankBoard // [ (i, S.singleton d) | (i, d) <- (givens text) ]

grid :: Board -> [Char]
grid b = intercalate divider [ band b i | i <- [0..2] ]
    where divider         = "\n------+-------+------\n"
          band b i        = intercalate "\n" [ row b r | r <- [i*3..(i*3)+2] ]
          row b r         = intercalate " | " [ row_chunk b r c | c <- [0, 3, 6] ]
          row_chunk b r c = intercalate " " [ [squareText (b ! s)] | s <- (take 3 (drop c (rows !! r))) ]

oneline :: Board -> [Char]
oneline = map squareText . toList

sideBySide :: Board -> Board -> [Char]
sideBySide g b =
    intercalate "\n" [ l1 ++ space ++ l2 | (l1, l2) <- zip (lines (grid g)) (lines (grid b)) ]
        where space = replicate 10 ' '

squareText :: S.Set Digit -> Char
squareText s = if S.size s == 1 then head (S.toList s) else '.'

-- Solving code ------------------------------------------------------

solve b = case emptySquare b of
            Nothing -> Just b
            Just s  -> tryDigits b s (S.toList (b ! s))

emptySquare b =
    if noEmpties then Nothing else Just fewestDigits
        where empties      = V.filter (\(i, s) -> S.size s > 1) $ indexed b
              noEmpties    = V.null empties
              fewestDigits = fst $ V.minimumBy setSize empties
              setSize a b  = compare (S.size $ snd a) (S.size $ snd b)

tryDigits b s [] = Nothing
tryDigits b s (d:ds) =
    case assign b s d of
      Nothing -> tryNextDigit
      Just b' -> solve b' <|> tryNextDigit
    where tryNextDigit = tryDigits b s ds

assign b s d = set (Just b) (s, d)

set b (s, d) = foldl eliminate b otherDigits
    where otherDigits = [ (s, d') | d' <- S.toList ((fromJust b) ! s), d' /= d ]

eliminate mb (s, d) =
    mb                    >>=
    removeDigit s d       >>=
    propagateAssignment s >>=
    propagateToOnlyPlace s d

removeDigit s d b =
    if wasLastDigit then Nothing else Just (b // [(s, newDigits)])
        where newDigits = S.delete d (b ! s)
              wasLastDigit = S.null newDigits

propagateAssignment s b
    | oneDigit = eliminateFromPeers theDigit
    | otherwise = Just b
    where oneDigit = S.size (b ! s) == 1
          theDigit = (S.toList (b ! s)) !! 0
          eliminateFromPeers d = foldl eliminate (Just b) [ (p, d) | p <- peersWith d ]
          peersWith d = [ p | p <- (peers !! s), canTake b p d ]

propagateToOnlyPlace s d b =
    foldl propagate (Just b) (units !! s)
        where propagate Nothing _ = Nothing
              propagate (Just b') u =
                  case places b' d u of
                    []   -> Nothing
                    x:[] -> set (Just b') x
                    _    -> (Just b')
              places b d u = [ (s, d) | s <- u, canTake b s d ]

canTake b s d = S.member d (b ! s)

-- Main --------------------------------------------------------------

main = do
  args <- getArgs
  forM_ args $ \a -> do
         puzzles <- readFile a
         forM_ (lines puzzles) $ \p -> do
             putStrLn $ (showSolution p)
             putStrLn ""

showSolution puzzle =
    case board puzzle of
      Nothing -> bail "Not a legal puzzle."
      Just p -> case solve p of
                  Nothing -> bail "No solution."
                  Just b -> sideBySide (justGivens puzzle) b
    where bail msg = (grid (justGivens puzzle)) ++ "\n" ++ msg
