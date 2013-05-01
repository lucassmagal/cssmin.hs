import System.Environment
import Text.Regex
import Control.Category

data WhereAmI = Code | InlineComment | BlockComment

main :: IO ()
main = do
    path:_ <- getArgs
    css <- readFile path

    putStrLn
      $ removeComments
      >>> removeNewLines
      >>> condenseWhitespace
      >>> removeUnnecessarySemicolons
      >>> condenseSemicolons
      $ css


removeComments :: String -> String
removeComments = loop Code where
    loop :: WhereAmI -> String -> String
    loop _ "" = ""

    -- match start of comments
    loop _ ('/':'*':remaining) = loop BlockComment remaining
    loop _ ('/':'/':remaining) = loop InlineComment remaining

    -- match end of comments
    loop BlockComment ('*':'/':remaining) = loop Code remaining
    loop InlineComment ('\n':remaining) = loop Code remaining

    -- match anything else
    loop Code (start:remaining) = start:loop Code remaining
    loop insideComment (_:remaining) = loop insideComment remaining

condenseWhitespace :: String -> String
condenseWhitespace css = subRegex (mkRegex "[ ]+") css " "

removeUnnecessarySemicolons :: String -> String
removeUnnecessarySemicolons css = subRegex (mkRegex ";+}") css "}"

condenseSemicolons :: String -> String
condenseSemicolons css = subRegex (mkRegex ";;+") css ";"

removeNewLines :: String -> String
removeNewLines css = subRegex (mkRegex "\n") css ""
