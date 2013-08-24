import Control.Monad
import Data.Map
import System.IO
import Text.Read

data Command = Quit
             | Assign String String
             | Report String

isQuit :: Command -> Bool
isQuit Quit = True
isQuit _ = False

instance Show Command where
    show Quit = "Quit"
    show (Assign var val) = "Assign " ++ var ++ " " ++ val
    show (Report var) = "Report " ++ var

type Environment = Map String Int

strip :: String -> String
strip = reverse . stripFront . reverse . stripFront
    where stripFront (' ':cs) = stripFront cs
          stripFront ('\t':cs) = stripFront cs
          stripFront cs = cs

parse :: String -> Command
parse input
    | strip input `elem` ["quit", "exit"]  = Quit
    | '=' `elem` input               = Assign varname value
    | otherwise                      = Report input
    where
        (varname, value) = (fst p, tail $ snd p)
        p = break (== '=') input

perform :: Command -> Environment -> IO Environment
perform (Assign var valtext) env =
    let val = readMaybe valtext in
    case val of
        Just v  -> return $ insert (strip var) v env
        Nothing -> do putStrLn $ "error: could not parse value '" ++ strip valtext ++ "'"
                      return env

perform (Report vartext) env =
    let var = strip vartext
        val = Data.Map.lookup var env in
    do case val of
           Just v -> print v
           Nothing -> putStrLn $ "error: no such variable '" ++ strip var ++ "'"
       return env

perform Quit _ = error "perform called with Quit"

repl :: Environment -> IO ()
repl e = do
    putStr ">> "
    hFlush stdout
    input <- getLine
    if (strip input == "")
        then repl e
        else let command = parse input in
             unless (isQuit command) $ perform command e >>= repl

main :: IO ()
main = repl empty
