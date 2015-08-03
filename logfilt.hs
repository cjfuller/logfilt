module Logfilt where

import Data.List (find, intercalate, sortBy, intersperse)
import Text.Regex (subRegex, splitRegex, mkRegex)
import Text.Regex.Posix ((=~), getAllTextMatches)

data Color = Green | Red | Yellow | Magenta | Cyan | Grey | Clear | Restore deriving (Eq, Enum, Bounded, Show, Read)

colorCode :: Color -> String
colorCode Red = "\x1b[31;1m"
colorCode Green = "\x1b[32;1m"
colorCode Yellow = "\x1b[33;1m"
colorCode Magenta = "\x1b[35;1m"
colorCode Cyan = "\x1b[36;1m"
colorCode Grey = "\x1b[30;1m"
colorCode Clear = "\x1b[0m"

filters:: [(String, Color)]
-- List of expressions that will be colorized in log lines.  Items higher up the
-- list should represent more specific rules.  They will embed correctly in the
-- context of colored regions lower down the list, but the reverse is not true.
filters = [
 ("INFO", Green),
 ("WARNING", Yellow),
 ("ERROR", Red),
 ("CRITICAL", Magenta),
 ("VALID", Green),
 ("INVALID", Yellow),
 -- Time and date info
 ("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[^]]*]", Grey),
 -- Colorize cyan to the end of line any log message starting with !!!
 -- Useful for quickly highlighting a specific debugging message in the logs.
 ("!!!.*$", Cyan)]

colorRe :: String
colorRe = ":(" ++ (intercalate "|" [show c | c <- [Green .. Clear]]) ++ "):"

formatColor :: Color -> String
formatColor c = ":" ++ (show c) ++ ":"

applyFilter :: (String, Color) -> String -> String
applyFilter (re, color) line = subRegex (mkRegex re) line ((formatColor color) ++ "\\0" ++ (formatColor Restore))

applyFilters :: [(String, Color)] -> String -> String
applyFilters [] s = s
applyFilters (f:rest) s = applyFilters rest $ applyFilter f s


nextToLastColor :: [Color] -> Color
nextToLastColor [] = Clear
nextToLastColor [_] = Clear
nextToLastColor (_:c:_) = c

extractColors :: String -> [Color]
extractColors s = map read $ map (filter ((/=) ':')) $ getAllTextMatches $ s =~ colorRe


restoreReplacementHelper :: String -> Color -> [String] -> String
restoreReplacementHelper building _ [] = building
restoreReplacementHelper building currColor (part:remaining)
    | part == formatColor Restore =
        restoreReplacementHelper (building ++ (formatColor currColor)) currColor remaining
    | otherwise =
        let newBuilding = building ++ part in
        let prevColor = nextToLastColor (extractColors newBuilding) in
        restoreReplacementHelper newBuilding prevColor remaining

concreteColorReplacementHelper :: [Color] -> String -> String
concreteColorReplacementHelper [] line = line
concreteColorReplacementHelper (c:remColors) line =
    concreteColorReplacementHelper remColors (subRegex (mkRegex $ formatColor c) line $ colorCode c)

doReplacements :: String -> String
doReplacements line =
    let parts = splitRegex (mkRegex $ formatColor Restore) line in
    concreteColorReplacementHelper [Green .. Clear] $
                                   restoreReplacementHelper "" Clear $
                                                            intersperse (formatColor Restore) parts

processLine :: String -> String
processLine = doReplacements . (applyFilters filters)

main =
    do
      input <- getContents
      putStr . unlines $ map processLine $ lines input
