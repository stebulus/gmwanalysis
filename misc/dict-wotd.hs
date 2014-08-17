import Data.Char
import Data.List
import Data.Maybe
import Data.Text (unpack)
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO
import Text.HTML.TagSoup

element :: [Tag a] -> [Tag a]
element xs =
    fst
    $ unzip
    $ takeWhile (\(_,n) -> n /= 0)
    $ zip xs
    $ [1] ++ (scanl1 (+) $ map delta xs)
    where delta (TagOpen _ _) = 1
          delta (TagClose _) = -1
          delta _ = 0

extract :: [Tag String] -> [String]
extract [] = []
extract tags =
    map (innerText . element)
    $ filter (\tags -> case tags of
                (TagOpen "a" attrs : _) ->
                    fromMaybe False
                    $ fmap (isPrefixOf "http://dictionary.reference.com/wordoftheday/archive/")
                    $ "href" `lookup` attrs
                _ -> False)
    $ map (drop 1)
    $ partitions (~== "<li>") tags

main = do
    filename:[] <- getArgs
    txt <- withFile filename ReadMode (fmap unpack . TIO.hGetContents)
    mapM_ putStrLn $ filter (/="") $ map (map toLower) $ extract $ parseTags txt
