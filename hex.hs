import Control.Monad
import Data.Functor
import Data.Map
import System.Exit
import System.IO
import Text.Read

data Command = Assign String String
             | Report String
             | NoOp

instance Show Command where
    show (Assign var val) = "Assign " ++ var ++ " " ++ val
    show (Report var) = "Report " ++ var
    show NoOp = "NoOp"

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
parse "" = NoOp
parse input
    | '=' `elem` input = Assign varname value
    | otherwise        = Report input
    where
        (varname, value) = (fst p, tail $ snd p)
        p = break (== '=') input

perform :: Command -> Environment -> (Environment, String)
perform (Assign var valtext) env =
    case readMaybe valtext of
        Just v  -> (updateEnv env (strip var) v, "")
        Nothing -> (env, "error: could not parse value '" ++ strip valtext ++ "'\n")

perform (Report vartext) env =
    let var = strip vartext
        val = lookupEnv env var in
    case val of
        Just v -> (env, show v ++ "\n")
        Nothing -> (env, "error: no such variable '" ++ var ++ "'\n")

perform NoOp env = (env, "")

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
    let (e', msg) = perform (parse input) e
    putStr msg
    repl e'

main :: IO ()
main = repl newEnv
