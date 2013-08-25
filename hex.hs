import Control.Monad
import Data.Functor
import Data.Map
import System.Exit
import System.IO
import Text.Read

data Command = Quit
             | Assign String String
             | Report String

instance Show Command where
    show Quit = "Quit"
    show (Assign var val) = "Assign " ++ var ++ " " ++ val
    show (Report var) = "Report " ++ var

type Environment = Map String Int

newEnv :: Environment
newEnv = Data.Map.empty

updateEnv :: Environment -> String -> Int -> Environment
updateEnv env var val = Data.Map.insert var val env

lookupEnv :: Environment -> String -> Maybe Int
lookupEnv = flip Data.Map.lookup

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
        Just v  -> return $ updateEnv env (strip var) v
        Nothing -> do putStrLn $ "error: could not parse value '" ++ strip valtext ++ "'"
                      return env

perform (Report vartext) env =
    let var = strip vartext
        val = lookupEnv env var in
    do case val of
           Just v -> print v
           Nothing -> putStrLn $ "error: no such variable '" ++ strip var ++ "'"
       return env

perform Quit _ = error "perform called with Quit"

isExit :: String -> Bool
isExit input
    | strip input `elem` ["exit", "quit"] = True
    | otherwise = False

repl :: Environment -> IO ()
repl e = do
    putStr ">> "
    hFlush stdout
    input <- strip <$> getLine
    when (isExit input) exitSuccess
    if input == ""
        then repl e
        else perform (parse input) e >>= repl

main :: IO ()
main = repl newEnv
