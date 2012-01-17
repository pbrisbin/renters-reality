import Prelude
import Yesod (GHandler)
import Text.Search.Sphinx
import Text.Search.Sphinx.Types

getResults :: String -> Sting -> Int -> Int -> GHandler s m (Maybe QueryResult)
getResults idx qterm offset limit = do
    res <- liftIO query (mkConfig offset limit)idx qterm
    putStrLn $ show res

config = defaultConfig
    { port = 9312
    , mode = Any
    }
