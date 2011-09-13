{-
    Zachary Weaver
    16/01/2011
    DictServer.hs
    A DICT server implementation
-}

{-
    module DictServer
        ( Dictionary(..)
        , Stategy
        , Database
        , DictServerError(..)
        , runServer
        ) where
-}

import qualified Data.Map as Map
import Network
import Control.Concurrent
import Control.Exception

type Strategy = String
type Database = String
data DictServerError = BadDB
                     | BadStrat

class Dictionary d where
    define      :: d -> Database -> String
                     -> IO Either [[String]] DictServerError
    match       :: d -> Database -> Strategy -> String
                     -> IO Either [(Database, String)] DictServerError 
    getDBs      :: d -> IO [Database]
    getStrats   :: d -> IO [Stategy]
    hasDB       :: d -> Database -> IO Bool
    hasDB dict db = getDBs dict >>= return . elem db
    hasStrat    :: d -> Stategy -> IO Bool
    hasStrat dict strat = getStrats dict >> = return . elem db

runServer :: Dictionary d => d -> IO ()
runServer dict = do
    clientsMVar <- newMVar Map.empty
    soc <- listenOn
    loop $ makeClient clientsMVar soc
        `catch` hndInterrupt
    where makeClient clientsMVar soc = do
                (h, _, _) <- accept soc
                exitfun <- newEmptyMVar
                tID <- forkIO $ serveClient exitfun h
                modifyMVar \clients -> return $ insert tId h clients
                putMVar exitfun (\h -> do
                    clients <- takeMVar clientsMVar
                    putMVar Map.delete clientsMVar
                    )

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

(a ?? b) c = if c then a else b

loop act = act >>= loop act ?? return ()

fromJust (Just a) = a
