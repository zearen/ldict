{- 
Zachary Weaver
07/01/11
DictClient.hs
A simple dict client

TODO: 
Make help

BONUS:
Better completion tools
Unified history file
-}

{-
module DictClient
    ( DictState
    , evalDict
    , define
    , match
    , setDB
    , setStrat
    , getDBs
    , getStrats
    , connect
    , quit
    ) where
-}

import Prelude hiding (catch)
import Data.Maybe (fromJust, maybe, isNothing)
import System (getArgs)
import Network (HostName)

import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.MonadException

import Dict

type DictState = StateT Dict (InputT IO)

------------------------------------------------------------
-- Client code
------------------------------------------------------------

main = do
    (hn, db, strat, def) <- getArgs >>= parseArgs
    dictServerName <- case hn of
        Nothing -> runInputT defaultSettings $
            liftM fromJust $ getInputLine "server: "
        Just url -> return url
    dict <- liftM (applyOptions db strat) $ dConnect dictServerName
    case def of
        [] -> evalDict (loop interface) dict
        word -> evalDict(doDefine word) dict >> return ()
    where applyOptions db strat = execState (do
                (return () `maybe` \d -> modify (\di -> di{database=d})) db
                (return () `maybe` \s -> modify (\di -> di{strategy=s})) strat
                ) -- Yeah, that just happenned

parseArgs [] = return (Nothing, Nothing, Nothing, [])
parseArgs ("--help":rest) = showHelp >> parseArgs rest
parseArgs [arg] = case arg of
     "-h" -> do
        putStrLn "Dict: expected server for -h"
        return (Nothing, Nothing, Nothing, [])
     "-d" -> do
        putStrLn "Dict: expected database for -d"
        return (Nothing, Nothing, Nothing, [])
     "-s" -> do
        putStrLn "Dict: expected strategy for -s"
        return (Nothing, Nothing, Nothing, [])
     word -> return (Nothing, Nothing, Nothing, [word])
parseArgs (arg:next:rest) = do
    (hn, db, strat, word) <- parseArgs rest
    case arg of
        "-h" -> return (Just next, db, strat, word)
        "-d" -> return (hn, Just next, strat, word)
        "-s" -> return (hn, db, Just next, word)
        nWord -> return (hn, db, strat, nWord : word)

interface :: DictState Bool
interface = do
    db <- liftM database get
    response <- ask (db ++ ": ")
    catch
      (parseResponse response) 
      onErr
    
    where 
      parseResponse response =
        case response of
            "quit" -> return False
            "help" -> doHelp []
            '<':newDB -> setDB newDB >> return True
            '$':newStrat -> setStrat newStrat >> return True
            '/':rest -> parseCommand commands $ words rest
            '@':com -> do
                let (db:word) = words com
                olddb <- liftM database get
                setDB db
                doDefine word
                setDB olddb
                return True
            '?':word -> doMatch $ words word
            '\\':word -> doDefine $ words word
            word -> doDefine $ words word
      onErr e = case e of
            DictResponseError _ _ -> say (show e) >> return False
            DictServerError _ -> say (show e) >> return False
            _ -> throwIO e

doDefine :: [String] -> DictState Bool
doDefine args = if null args
    then do
        say "Dict: Missing argument <word> for define"
        return True
    else
        catch
            (define (unwords args) >>= mapM_ (mapM_ say) >> return True)
            onErr
    where onErr e = case e of
              DictBadDB _ -> say (show e) >> return True
              _ -> throwIO e
    

doMatch :: [String] -> DictState Bool
doMatch args = if null args
    then do
        say "Dict: Missing argument <word> for match"
        return True
    else
        catch
          (match (unwords args) >>= mapM_ 
            (\(db, mtch) -> say (db ++ ": " ++ mtch)) >> return True)
        onErr
    where onErr e = case e of
              DictBadDB _ -> say (show e) >> return True
              DictBadStrat _ -> say (show e) >> return True
              _ -> throwIO e

doShow args = parseCommand showComs args

showComs = 
    [ ("dbs", getFromServer getDBs)
    , ("databases", getFromServer getDBs)
    , ("strats", getFromServer getStrats)
    , ("strategies", getFromServer getStrats)
    , ("db", getFromState database)
    , ("database", getFromState database)
    , ("strat", getFromState strategy)
    , ("strategy", getFromState strategy)
    , ("server", getFromState serverName)
    ]
    where getFromServer getFun _ = do 
              things <- getFun 
              mapM_ (\ (thing, desc) -> say $ thing ++ ' ' : desc) things
              return True
          getFromState getFun _ = get >>= say . getFun >> return True

doSet args = parseCommand setComs args

setComs = 
    [ ("db", insistHead setDB)
    , ("database", insistHead setDB)
    , ("strategy", insistHead setStrat)
    ]
    where insistHead setFun args = if null args
              then do
                  say "Dict: Missing argument <newval> for set"
                  return True
              else setFun (head args) >> return True

parseCommand :: [(String, [String] -> DictState Bool)] -> [String]
    -> DictState Bool
parseCommand coms args = if null args
    then do
        say "Dict: Missing command or argument"
        return True
    else do
        let arg = head args
        let matches = matchCommand arg $ map fst coms
        case matches of
            [] -> do
                say ("Dict: \"" ++ arg ++ "\" did not match any known command")
                return True
            [com] -> (fromJust $ lookup com coms) $ tail args
            comds -> if null $ filter (arg==) comds
                then do
                    say $ "Dict: Ambiguous command or argument \"" ++ arg 
                        ++ "\""
                    say $ "Possible completions:" 
                        ++ foldr (\x a -> ' ' : a ++ x) "" args
                    return True
                else (fromJust $ lookup arg coms) $ tail args

commands :: [(String, [String] -> DictState Bool)]
commands =
    [ ("show", doShow)
    , ("define", doDefine)
    , ("match", doMatch)
    , ("set", doSet)
    , ("help", doHelp)
    , ("quit", \_ -> return False)
    ]

-- There are faster ways, but meh
matchCommand :: String -> [String] -> [String]
matchCommand test = filter (\s -> length s >= len && and (zipWith (==) s test))
    where len = length test

------------------------------------------------------------
-- State Monad tools
------------------------------------------------------------                    

ask :: String -> DictState String
ask prompt = lift (getInputLine prompt) >>= \maybeWord ->
    case maybeWord of
        Nothing -> return []
        Just word -> return word

say :: String -> DictState ()
say = lift . outputStrLn

-- Handles openning a connection and quits after done

evalDict :: DictState a -> Dict -> IO a
evalDict state dict = do
    runInputT defaultSettings (evalStateT (do
        a <- state
        quit
        return a
        ) dict
       )

define :: String -> DictState [[String]]
define word = get >>= \d -> lift . lift $ dDefine d word

match :: String -> DictState [(Database, String)]
match word = get >>= \d -> lift . lift $ dMatch d word

setDB :: String -> DictState ()
setDB = modify . dSetDB

setStrat :: String -> DictState ()
setStrat = modify . dSetStrat

getDBs :: DictState [(Database, String)]
getDBs = get >>= lift . lift . dGetDBs

getStrats :: DictState [(Strategy, String)]
getStrats = get >>= lift . lift . dGetStrats

-- Remember to quit first!
connect :: String -> DictState ()
connect hn = (lift . lift) (dConnect hn) >>= put

quit :: DictState ()
quit = get >>= lift . lift . dQuit

------------------------------------------------------------
-- Other Utilities
------------------------------------------------------------

(a ?? b) tf = if tf then a else b

loop act = act >>= loop act ?? return ()

------------------------------------------------------------
-- Help diplays
------------------------------------------------------------

showHelp = mapM_ putStrLn
    [ "USAGE: DictClient [-h server | -d database | -s strategy | --help]* [word]"
    , "-h      Server to connect to"
    , "-d      First database to use"
    , "-s      First strategy to use"
    , "--help  Display this help"
    , ""
    , "If a word is specified, a single lookup is performed.  Otherwise, the client"
    , "enters interactive mode"
    ]

doHelp _ = mapM_ say
    [ "The prompt always displays the current target database"
    , "To define a word, type the word in followed by enter"
    , "quit and help are special words, when they are entered, the program quits or"
    , "    displays this help respectively"
    , "If certain characters predicate a word, it will not be defined and a special"
    , "    function will be performed."
    , "<db         Sets the database to db"
    , "$strat      Sets the current strategy to strat"
    , "?word       Matches word using current strategy"
    , "@db word    Looks up word in db without changing current db"
    , "\\word      Escapes a word to be defined, IE \\quit defines quit"
    , "/command    Runs given command"
    , ""
    , "Commands"
    , "show    db|database     Show current database"
    , "        dbs|databases   Show avalible databases"
    , "        strategy        Show current strategy"
    , "        strategies      Show availible strategies"
    , "        server          Show hostname of server"
    , "define                  Defines given word"
    , "match                   Matches given word"
    , "set     db|database     Set current database to given database"
    , "        strategy        Set current strategy to given strategy"
    , "help                    Show this help"
    , "quit                    Disconnect and quit"
    ] >> return True
