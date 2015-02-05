import Text.Regex.PCRE

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

colorize :: Color -> String -> String

colorize color str = (colorCode color) ++ str ++ (colorCode Clear)


maybeColorizeLine :: Color -> String -> String -> String

maybeColorizeLine color re line =
    if line =~ re then
        colorize color line
    else
        line

maybeColorizePartial :: Color -> String -> String -> String

maybeColorizePartial color re line
    | line == "" = ""
    | otherwise = let (before, match, after) = (line =~ re)
                  in before ++ (colorize color match) ++
                     (maybeColorizePartial color re after)

-- TODO: want to have a way to deal with overlapping colors e.g. if we color
-- the whole line cyan, coloring the INFO green shouldn't then turn the rest of
-- the text back to the default color.
matchers :: [String -> String]

matchers = [
 (maybeColorizePartial Green "INFO"),
 (maybeColorizePartial Yellow "WARNING"),
 (maybeColorizePartial Red "ERROR"),
 (maybeColorizePartial Magenta "CRITICAL"),
 -- Time and date info
 (maybeColorizePartial Grey "\\d{4}-\\d{2}-\\d{2}.*]"),
 -- Colorize cyan to the end of line any log message starting with !!!
 -- Useful for quickly highlighting a specific debugging message in the logs.
 (maybeColorizePartial Cyan "!!!.*$")]


main =
    let matcherFn = foldl1 (\f0 f1 -> f0 . f1) matchers
    in do
      input <- getContents
      putStr . unlines $ map matcherFn $ lines input
