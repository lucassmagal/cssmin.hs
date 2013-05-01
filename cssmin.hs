import System.Environment
import Text.Regex
import Control.Category


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


removeComments css = removeComments' css "" where
    removeComments' "" _ = ""

    -- match start of comments
    removeComments' ('/':'*':tail) _ = removeComments' tail "*/"
    removeComments' ('/':'/':tail) _ = removeComments' tail "\n"

    -- match end of comments
    removeComments' ('*':'/':tail) "*/" = removeComments' tail ""
    removeComments' ('\n':tail) "\n"    = removeComments' tail ""

    -- match anything else
    removeComments' (head:tail) ""      = head:removeComments' tail ""
    removeComments' (head:tail) stop_on = removeComments' tail stop_on

condenseWhitespace css = subRegex (mkRegex "[ ]+") css " "

removeUnnecessarySemicolons css = subRegex (mkRegex ";+}") css "}"

condenseSemicolons css = subRegex (mkRegex ";;+") css ";"

removeNewLines css = subRegex (mkRegex "\n") css ""
