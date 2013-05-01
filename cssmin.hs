import System.Environment
import Text.Regex
import Control.Category

data WhereAmI = Code | InlineComment | BlockComment

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


removeComments css = removeComments' css Code where
    removeComments' "" _ = ""

    -- match start of comments
    removeComments' ('/':'*':remaining) _ = removeComments' remaining BlockComment
    removeComments' ('/':'/':remaining) _ = removeComments' remaining InlineComment

    -- match end of comments
    removeComments' ('*':'/':remaining) BlockComment = removeComments' remaining Code
    removeComments' ('\n':remaining) InlineComment   = removeComments' remaining Code

    -- match anything else
    removeComments' (start:remaining) Code      = start:removeComments' remaining Code
    removeComments' (start:remaining) whereAmI  = removeComments' remaining whereAmI

condenseWhitespace css = subRegex (mkRegex "[ ]+") css " "

removeUnnecessarySemicolons css = subRegex (mkRegex ";+}") css "}"

condenseSemicolons css = subRegex (mkRegex ";;+") css ";"

removeNewLines css = subRegex (mkRegex "\n") css ""
