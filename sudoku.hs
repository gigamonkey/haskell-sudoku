import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Vector ((//), (!))
import System.Environment
import System.IO

import qualified Data.Set as S
import qualified Data.Vector as V

type Square = Int
type Digit  = Char
type Givens = [(Square, Digit)]
type Board  = V.Vector (S.Set Digit)

-- Some useful data --------------------------------------------------

digits  = "123456789"
squares = [0..80]

blankSquare = S.fromList digits
blankBoard  = V.replicate 81 blankSquare

rows     = [ [ r*9 + c | c <- [0..8] ] | r <- [0..8] ]
cols     = [ [ r*9 + c | r <- [0..8] ] | c <- [0..8] ]
boxes    = [ [ r*9 + c | r <- [rs..rs+2], c <- [cs..cs+2] ] | rs <- [0,3,6], cs <- [0,3,6] ]
allUnits = rows ++ cols ++ boxes

units = [ [ u | u <- allUnits, s `elem` u ] | s <- squares ]
peers = [ delete s (foldl union [] (units !! s)) | s <- squares ]

-- To and from textual representation --------------------------------

givens :: String -> Givens
givens text = [ (i, d) | (i, d) <- zip [0..] (fromText text), d `elem` digits ]
    where fromText = filter (`elem` ('.':digits))

board :: String -> Maybe Board
board text = foldl setter (Just blankBoard) (givens text)
    where setter mb g = mb >>= set g

justGivens :: String -> Board
justGivens text = blankBoard // [ (i, S.singleton d) | (i, d) <- givens text ]

grid :: Board -> String
grid b =
    intercalate divider $ map band $ group 3 $ group 9 squares
    where squares    = map squareText $ V.toList b
          group n xs = case xs of [] -> []; _ -> take n xs : group n (drop n xs)
          chunk      = intersperse ' '
          row r      = intercalate " | " $ map chunk $ group 3 r
          band b     = intercalate "\n" $ map row b
          divider    = "\n------+-------+------\n"

oneline :: Board -> String
oneline = map squareText . V.toList

sideBySide :: Board -> Board -> String
sideBySide g b =
    intercalate "\n" $ zipWith line (lines $ grid g) (lines $ grid b)
        where line l1 l2 = l1 ++ replicate 10 ' ' ++ l2

squareText :: S.Set Digit -> Char
squareText s = if S.size s == 1 then head (S.toList s) else '.'

-- Solving code ------------------------------------------------------

solve b = case emptySquare b of
            Nothing -> Just b
            Just s  -> tryDigits b s (S.toList (b ! s))

emptySquare b =
    if noEmpties then Nothing else Just fewestDigits
        where empties      = V.filter isEmpty $ V.indexed b
              isEmpty      = (> 1) . S.size . snd
              noEmpties    = V.null empties
              fewestDigits = fst $ V.minimumBy setSize empties
              setSize a b  = compare (S.size $ snd a) (S.size $ snd b)

tryDigits b s [] = Nothing
tryDigits b s (d:ds) =
    case assign b s d of
      Nothing -> tryNextDigit
      Just b' -> solve b' <|> tryNextDigit
    where tryNextDigit = tryDigits b s ds

assign b s d = set (s, d) b

set (s, d) b = foldl eliminate (Just b) otherDigits
    where otherDigits = [ (s, d') | d' <- S.toList (b ! s), d' /= d ]

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
          theDigit = head $ S.toList (b ! s)
          eliminateFromPeers d = foldl eliminate (Just b) [ (p, d) | p <- peersWith d ]
          peersWith d = [ p | p <- peers !! s, canTake b p d ]

propagateToOnlyPlace s d b =
    foldl propagate (Just b) (units !! s)
        where propagate Nothing _ = Nothing
              propagate (Just b') u =
                  case places b' d u of
                    []   -> Nothing
                    x:[] -> set x b'
                    _    -> Just b'
              places b d u = [ (s, d) | s <- u, canTake b s d ]

canTake b s d = S.member d (b ! s)

-- Main --------------------------------------------------------------

main = do
  args <- getArgs
  forM_ args $ \a -> do
         puzzles <- readFile a
         forM_ (lines puzzles) $ \p -> do
                       putStrLn $ showSolution p
                       putStrLn ""

showSolution puzzle =
    case board puzzle of
      Nothing -> bail "Not a legal puzzle."
      Just p -> case solve p of
                  Nothing -> bail "No solution."
                  Just b -> sideBySide (justGivens puzzle) b
    where bail msg = grid (justGivens puzzle) ++ "\n" ++ msg
