import DictServer
import qualified Data.Map as Map

type Map = Map.Map
type MyDict = Map Database (Map String)

instance Dictionary MyDict where
    define 
