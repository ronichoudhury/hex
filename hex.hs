import Control.Monad
import Data.Functor
import Data.Map
import System.Exit
import System.IO
import Text.Read

-- The different actions the user can request from the system.
data Command = Assign String String
             | Report String
             | NoOp

-- For debugging purposes - prints out a text representation of a Comand value.
instance Show Command where
    show (Assign var val) = "Assign " ++ var ++ " " ++ val
    show (Report var) = "Report " ++ var
    show NoOp = "NoOp"

-- The environment maps names to (int) values.
type Environment = Map String Int

-- All new environments are empty.
newEnv :: Environment
newEnv = Data.Map.empty

-- Create a new name-value pair, or update an existing one.
updateEnv :: Environment -> String -> Int -> Environment
updateEnv env var val = Data.Map.insert var val env

-- If a name exists in the environment, return its value.
lookupEnv :: Environment -> String -> Maybe Int
lookupEnv = flip Data.Map.lookup

-- Remove surrounding whitespace from a string.
strip :: String -> String
strip = reverse . stripFront . reverse . stripFront
    where stripFront (' ':cs) = stripFront cs
          stripFront ('\t':cs) = stripFront cs
          stripFront cs = cs

-- Determine the semantic meaning of an input line.
parse :: String -> Command
parse "" = NoOp
parse input
    | '=' `elem` input = Assign varname value
    | otherwise        = Report input
    where
        (varname, value) = (fst p, tail $ snd p)
        p = break (== '=') input

-- Compute appropriate output/error message/etc., and a new environment - both
-- as a result of applying a command to the current environment.
perform :: Command -> Environment -> (Environment, String)
perform (Assign var valtext) env =
    case readMaybe valtext of
        Just v  -> (updateEnv env (strip var) v, "")
        Nothing -> (env, "error: could not parse value '" ++ strip valtext ++ "'\n")

perform (Report vartext) env =
    let var = strip vartext in
    case lookupEnv env var of
        Just value -> (env, show value ++ "\n")
        Nothing    -> (env, "error: no such variable '" ++ var ++ "'\n")

perform NoOp env = (env, "")

-- Detect whether a (stripped) input line is an exit command.
isExit :: String -> Bool
isExit input
    | strip input `elem` ["exit", "quit"] = True
    | otherwise = False

-- The REPL - get input, parse, apply to current environment and then "loop"
-- with an updated environment.
repl :: Environment -> IO ()
repl e = do
    -- Print the prompt.
    putStr ">> "
    hFlush stdout

    -- Get the input, and strip it of surrounding whitespace.
    input <- strip <$> getLine

    -- Exit now if requested.
    when (isExit input) exitSuccess

    -- Parse the input for command value, generate messages and a new
    -- evironment, print the messages, and run the repl again with the new
    -- environment.
    let (e', msg) = perform (parse input) e
    putStr msg
    repl e'

-- Pass control to the REPL, supplying it an initial, empty environment.
main :: IO ()
main = repl newEnv
