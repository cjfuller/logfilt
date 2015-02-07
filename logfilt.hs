import Data.Array (elems)
import Data.List (find, intercalate, sortBy)
import Text.Regex.PCRE ((=~), MatchArray)

data Color = Green | Red | Yellow | Magenta | Cyan | Grey | Clear deriving (Eq, Enum, Bounded, Ord, Show, Read)

colorCode :: Color -> String

colorCode color
    | color == Red = "\x1b[31;1m"
    | color == Green = "\x1b[32;1m"
    | color == Yellow = "\x1b[33;1m"
    | color == Magenta = "\x1b[35;1m"
    | color == Cyan = "\x1b[36;1m"
    | color == Grey = "\x1b[30;1m"
    | color == Clear = "\x1b[0m"


escapeColorCode :: String -> String
-- Escape the [ within the color codes so they can be used in regular expressions.
escapeColorCode "" = ""
escapeColorCode (c0:rem)
    | c0 == '[' = "\\[" ++ escapeColorCode rem
    | otherwise = [c0] ++ escapeColorCode rem


codeRe = "(" ++ (intercalate "|" $ map (escapeColorCode . colorCode) [Green .. Clear]) ++ ")"


colorFromCode :: String -> Color
-- Reverse the colorCode function and get a Color from the string code.
-- Default to Clear if it's not a known color code.
colorFromCode code = case find (\color -> (colorCode color) == code) [Green .. Clear] of
                       Nothing -> Clear
                       Just c -> c


slice :: Int -> Int -> String -> String
-- Slice a String, starting at a given position and having a given length.
slice start len str = fst $ splitAt len (snd $ splitAt start str)


getRestoreColor :: String -> Int -> Color
-- Find the current color as of a specified position in a string.
-- When we're adding additional colors on top, this will be the color to which
-- we restore.
getRestoreColor line newColorPos =
    let existingColors :: [MatchArray]
        existingColors = (line =~ codeRe)
    -- This finds the color tags in the string before the current position.
    in case takeWhile (\(i, _) -> i < newColorPos) $ map (head . elems) existingColors of
         [] -> Clear
         matches -> let (start, len) = last matches
                    in colorFromCode $ slice start len line


colorize :: Color -> Color -> String -> String
-- Colorize a string with a specified color, returning to the specified
-- restoreColor afterwards.
colorize color restoreColor str = (colorCode color) ++ str ++ (colorCode restoreColor)


maybeColorizeLine :: Color -> String -> String -> String
-- Colorize an entire string if it matches a specified regular expression.
maybeColorizeLine color re line =
    if line =~ re then
        colorize color Clear line
    else
        line


colorizationFoldFunction ::
    Color -> String -> (Int, String) -> (Int, Int) -> (Int, String)
-- Function that when curried serves as the fold function for parial colorization.
-- Curry with the color being added and the (complete) line being colorized.
--
-- The resulting fold function uses an accumulator (lastPos, str) that contains
-- the last position in the input string that has been processed, and the string
-- result from colorizing up to that point.  The current element for the fold
-- function is the segment (start, len) indexed to the input string to which to
-- apply the color.
colorizationFoldFunction color line acc curr =
    let before = (snd acc) ++ (slice (fst acc) ((fst curr) - (fst acc)) line)
        restoreColor = getRestoreColor before (length before)
    in ((fst curr) + (snd curr)
       ,before ++ (colorize color restoreColor (slice (fst curr) (snd curr) line)))


colorizationREMatches :: String -> String -> [(Int, Int)]
-- Retrieve a list of all (start, len) matches to a given regular expression.
colorizationREMatches line re =
    let matches :: [MatchArray]
        matches = (line =~ re)
    in map (head . elems) matches


maybeColorizePartial :: Color -> String -> String -> String
-- Colorize all segments of a string matching a given regular expression.
maybeColorizePartial color re line =
    let matchList = colorizationREMatches line re
        (endIndex, coloredString) =
            (foldl (colorizationFoldFunction color line) (0, "")
                   matchList)
    in coloredString  ++ (snd $ splitAt endIndex line)


matchers :: [String -> String]
-- List of matchers that will be colorized in log lines.  Items higher up the
-- list should represent more specific rules.  They will embed correctly in the
-- context of colored regions lower down the list, but the reverse is not true.
matchers = [
 (maybeColorizePartial Green "INFO"),
 (maybeColorizePartial Yellow "WARNING"),
 (maybeColorizePartial Red "ERROR"),
 (maybeColorizePartial Magenta "CRITICAL"),
 -- Time and date info
 (maybeColorizePartial Grey "\\d{4}-\\d{2}-\\d{2}[^]]*]"),
 -- Colorize cyan to the end of line any log message starting with !!!
 -- Useful for quickly highlighting a specific debugging message in the logs.
 (maybeColorizeLine Cyan "!!!.*$")]


main =
    let matcherFn = foldl1 (\f0 f1 -> f0 . f1) matchers
    in do
      input <- getContents
      putStr . unlines $ map matcherFn $ lines input
