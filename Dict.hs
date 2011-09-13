{-# LANGUAGE DeriveDataTypeable #-}

{-
Zachary Weaver
02/01/2011
Dict.hs
A basic library for the dict protocol
-}

module Dict 
    ( Dict (database, strategy, serverName)
    , DictException(..)
    , Database
    , Strategy
    , dConnect 	    -- HostName -> IO Dict
    , dSetDB        -- Database -> Dict -> Dict
    , dSetStrat     -- Strategy  -> Dict -> Dict
    , dDefine       -- Dict -> IO [[String]]
    , dMatch        -- Dict -> IO [(Database, String)]
    , dGetDBs       -- Dict -> IO [(Database, String)]
    , dGetStrats    -- Dict -> IO [(Strategy, String)]
    , dQuit         -- Dict -> IO ()
    ) where


import System.IO
import Network
import Data.Typeable
import Control.Exception

type Database = String
type Strategy = String

data Dict = Dict
    { connection :: Handle
    , database   :: Database
    , strategy   :: Strategy
    , serverName :: String
    }
    deriving (Eq, Show)

data DictException = DictConnectionError
                   | DictResponseError String String
                   | DictBadDB String
                   | DictBadStrat String
                   | DictServerError String
                deriving (Typeable)

instance Show DictException where
    show e = "Dict: " ++ showEach e
        where showEach DictConnectionError = "Connection failed"
              showEach (DictResponseError exp got) = 
                  ("Expecting " ++) . shows exp . (", got " ++) $ show got
              showEach (DictBadDB db) = "Invalid database: " ++ db
              showEach (DictBadStrat strat) = "Invalid strategy: " ++ strat
              showEach (DictServerError msg) = "Server " ++ msg
instance Exception DictException

dConnect :: HostName -> IO Dict
dConnect url = do
    con <- connectTo url $ PortNumber 2628
    hSetBuffering con LineBuffering
    hSetNewlineMode con $ NewlineMode CRLF CRLF
    header <- hGetLine con
    if "220" == take 3 header
      then return ()
      else connectFail con
    {-
    hPutStrLn con "CLIENT xLambdaDict"
    response <- hGetLine con
    if "250" == take 3 response
      then return ()
      else connectFail con
    -}
    return $ Dict con "!" "exact" url
    where connectFail con = do
              hClose con
              throw DictConnectionError

dSetDB :: Database -> Dict -> Dict
dSetDB db dict = dict{database=db}

dSetStrat :: Strategy -> Dict -> Dict
dSetStrat strat dict = dict{strategy=strat}

dDefine :: Dict -> String -> IO [[String]]
dDefine dict word = do
    dSend dict $ "DEFINE " ++ database dict ++ ' ' : show word
    (num, rest) <- liftM (splitAt 3) $ dGetLine dict
    case num of
        "550" -> throw $ DictBadDB $ database dict
        -- Until lojban.org is fixed
        "501" -> throw $ DictBadDB $ database dict
        "552" -> return []
        "150" -> do
            let [(numDefs,_)] = reads rest
            defs <- nTimesM numDefs (assertResponse dict "151" 
                >> dGetBlock dict)
            assertResponse dict "250"
            return defs

dMatch :: Dict -> String -> IO [(Database, String)]
dMatch dict word = do
    dSend dict $ "MATCH " ++ database dict ++ ' ' : strategy dict 
        ++ ' ' : show word
    num <- dGetNum dict
    case num of
        "550" -> throw $ DictBadDB $ database dict
        "551" -> throw $ DictBadStrat $ strategy dict
        -- Until lojban.org is fixed
        "501" -> throw $ DictBadDB $ database dict
        "552" -> return []
        "152" -> do
            matches <- dGetBlock dict
            assertResponse dict "250"
            return $ parseBlock matches

dGetDBs :: Dict -> IO [(Database, String)]
dGetDBs dict = do
    dSend dict "SHOW DB"
    num <- dGetNum dict
    case num of
        "554" -> return []
        "110" -> do
            dbs <- dGetBlock dict
            assertResponse dict "250"
            return $ parseBlock dbs

dGetStrats :: Dict -> IO [(Strategy, String)]
dGetStrats dict = do
    dSend dict "SHOW STRAT" -- Check me
    num <- dGetNum dict
    case num of
        "555" -> return []
        "111" -> do
            strats <- dGetBlock dict
            assertResponse dict "250"
            return $ parseBlock strats

dQuit :: Dict -> IO ()
dQuit dict = do
    dSend dict "QUIT"
    num <- dGetNum dict
    if num == "221"
      then return ()
      else throw $ DictResponseError "221" num
    hClose $ connection dict

------------------------------------------------------------
-- Basic communication
------------------------------------------------------------

assertResponse :: Dict -> String -> IO ()
assertResponse dict num = do
    msg <- dGetNum dict
    if and $ zipWith (==) num msg
      then return ()
      else do 
          dQuit dict
          throw $ DictResponseError num msg

dSend :: Dict -> String -> IO ()
dSend dict str = hPutStrLn (connection dict) str


dGetNum :: Dict -> IO String
dGetNum = liftM (take 3) . dGetLine

dGetLine :: Dict -> IO String
dGetLine dict = do
    line <- hGetLine $ connection dict
    case line of
        '4':'2':'0':_ -> throw $ DictServerError "unavailible"
        '4':'2':'1':_ -> throw $ DictServerError "shutting down"
        _ -> return line

dGetBlock :: Dict -> IO [String]
dGetBlock dict = do
    line <- dGetLine dict
    if safeHeadIs '.' line
      then return []
      else do
          rest <- dGetBlock dict
          return $ line : rest

-- FIXME I don't parse right
parseBlock = map ((\(db,word)->(db,clean word)) . span (/=' '))
    where clean = filter (/='"') . dropWhile (==' ')

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

nTimesM :: (Integral i, Monad m) => i -> m a -> m [a]
nTimesM 0 _ = return []
nTimesM n act = do
    result <- act
    rest <- nTimesM (n-1) act
    return $ result:rest

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mon = mon >>= return . f

safeHeadIs :: Eq a => a -> [a] -> Bool
safeHeadIs _ [] = False
safeHeadIs val (h:_) = h == val
