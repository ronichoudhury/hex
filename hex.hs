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
strip s = reverse . stripFront . reverse $ stripFront s
    where stripFront "" = ""
          stripFront (' ':cs) = stripFront cs
          stripFront ('\t':cs) = stripFront cs
          stripFront cs = cs

parse :: String -> Command
parse input
    | input `elem` ["quit", "exit"]  = Quit
    | '=' `elem` input               = Assign varname (tail value)
    | otherwise                      = Report input
    where
        (varname, value) = break (== '=') input

perform :: Command -> Environment -> IO Environment
perform cmd env = do
    case cmd of
        Assign var valtext ->
            let val = readMaybe valtext in
               case val of
                   Just v -> return $ insert (strip var) v env
                   Nothing -> do putStrLn $ "error: could not parse value '" ++ (strip valtext) ++ "'"
                                 return env
        Report vartext -> let var = strip vartext
	                      val = Data.Map.lookup var env in
                          do case val of
                                 Just v -> putStrLn $ show v
                                 Nothing -> putStrLn $ "error: no such variable '" ++ (strip var) ++ "'"
                             return env
        Quit -> error "perform called with Quit"

repl :: Environment -> IO ()
repl e = do
    putStr ">> "
    hFlush stdout
    input <- getLine
    if strip input == ""
        then repl e
        else do command <- return $ parse input
	        if isQuit command
                    then return ()
                    else do e' <- perform command e
			    repl e'

main :: IO ()
main = repl empty
