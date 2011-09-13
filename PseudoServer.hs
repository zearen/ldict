import IO
import System.IO.Error
import Network
import Control.Concurrent

import System (getArgs)

main = do
    args <- getArgs
    port <- liftM (fromInteger . read) (if null args 
        then do
            prompt "Port: "
            getLine
        else return $ head args)
    listenOn (PortNumber port) >>= runServer

runServer :: Socket -> IO ()
runServer soc = do
    prompt "Waiting... "
    (con,_,_) <- accept soc
    hSetBuffering con LineBuffering
    mainID <- myThreadId
    forkIO $ loop $ catch (reader con) (\e -> throwTo mainID e >> return False)
    putStrLn "Connected"
    loop $ catch (writer con) onErr
    
    where reader con = do
              line <- hGetLine con
              prompt "  > "
              putStrLn line
              yield
              return True
          writer con = do
              getLine >>= hPutStrLn con
              yield
              return True
          onErr e = do
              putStrLn "Connection terminated"
              return False

prompt :: String -> IO ()
prompt str = putStr str >> hFlush stdout

loop :: IO Bool -> IO ()
loop act = act >>= loop act ?? return ()

liftM f mon = mon >>= return . f

(f ?? g) b = if b then f else g
