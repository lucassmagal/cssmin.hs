import System.Environment
import Text.Regex


-- This operator will pipe the result of one function
-- to another. "f1 ==> f2" is the same of "f2 (f1)"
-- Basically, it is the opposite of "$"
(==>) = flip ($)


main = do
    args <- getArgs
    css <- readFile $ args !! 0

    putStrLn $ removeComments css ==>
               removeNewLines ==>
               condenseWhitespace ==>
               removeUnnecessarySemicolons ==>
               condenseSemicolons


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
