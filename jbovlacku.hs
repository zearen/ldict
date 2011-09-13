{-
Zachary Weaver AKA Zearen Wover 
Do whatever you want with it, just attribute it back to me
-}

import IO
import System (getArgs)
import System.Process


modes = [
    "gismu",
    "rafsi",
    "lujvo",
    "cmavo",
    "@jbo",
    "@gli"]

main = do
    args <- getArgs
    parseArgs args

parseArgs :: [String] -> IO ()
parseArgs []     = mainloop "@jbo"
parseArgs ("--help":rest) = showHelp >> return ()
parseArgs (arg:rest)
    | arg `elem` modes && (not . null) rest = actOn arg $ head rest
    | otherwise = actOn "@jbo" arg

mainloop :: String -> IO ()
mainloop mode = do
    putStr $ mode ++ "> "
    hFlush stdout
    instr <- getLine
    if null instr
        then mainloop mode
        else
            case instr of
                ('<':newMode) -> changeMode mode newMode
                "help"   -> do
                    showHelp
                    mainloop mode
                "exit"   -> return ()
                instr    -> do 
                    actOn mode instr
                    mainloop mode

changeMode :: String -> String -> IO ()
changeMode oldMode newMode = if newMode `elem` modes
    then mainloop newMode
    else do
        putStrLn $ "Invalid mode \"" ++ newMode ++ "\""
        mainloop oldMode

actOn :: String -> String -> IO ()
actOn mode instr = runCmd $ chooser mode
    where chooser "@jbo"  =
            "dict -h www.lojban.org -d 'jbo->en' " ++ instr
          chooser "@gli"  =
            "dict -h www.lojban.org -d 'en->jbo' " ++ instr
          chooser "rafsi" =
            "grep \"^"  ++ instr ++ "\" /usr/share/lojban/rafsi.txt"
          chooser "lujvo" =
            "grep \"^"  ++ instr ++ " \" /usr/share/lojban/lujvo.txt"
          chooser "gismu" =
            "grep \"^ " ++ instr ++ "\" /usr/share/lojban/gismu.txt"
          chooser "cmavo" =
            "grep \"^"  ++ groom instr ++ " \" /usr/share/lojban/cmavo.txt"
            where groom [] = []
                  groom word@('.' : rest) = word
                  groom word
                      | head word `elem` "aeiou" = '.' : word
                      | otherwise                = ' ' : word

runCmd cmd = do
    pid <- runCommand cmd
    waitForProcess pid
    return ()

helpMesg = [
    "Usage: jbovlacku [[mode] query]",
    "",
    "Searches for query in given mode.  If no mode is provided, it is assumed to be",
    "@jbo.  If neither mode or query are provided, then jbovlacku enters its ",
    "interactive mode.",
    "",
    "Interactive commands",
    "<query>' : Searches for query in current mode",
    "/<mode> : Changes to given mode",
    "help    : Displays this menu",
    "exit    : Quits interactive mode",
    "",
    "Modes",
    "@jbo  : Searches the Lojban to English jbovlaste dictionary",
    "@gli  : Searches the English to Lojban jbovlaste dictionary",
    "gismu : Searches for gismu",
    "rafsi : Searches for rafsi",
    "cmavo : Searches for cmavo",
    "lujvo : Searches for lujvo"]

showHelp = mapM putStrLn helpMesg
