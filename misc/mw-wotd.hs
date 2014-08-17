{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Text (Text, toLower, unpack, isPrefixOf)
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO
import Text.HTML.TagSoup

element :: [Tag Text] -> [Tag Text]
element xs =
    fst
    $ unzip
    $ takeWhile (\(_,n) -> n /= 0)
    $ zip xs
    $ [1] ++ (scanl1 (+) $ map delta xs)
    where delta (TagOpen _ _) = 1
          delta (TagClose _) = -1
          delta _ = 0

extract :: [Tag Text] -> [Text]
extract [] = []
extract tags =
    map (innerText . element)
    $ partitions (\tag -> case tag of
                    TagOpen "a" attrs ->
                        fromMaybe False
                        $ fmap (isPrefixOf "/word-of-the-day/") ("href" `lookup` attrs)
                    _ -> False)
                 tags

main = do
    filename:[] <- getArgs
    txt <- withFile filename ReadMode TIO.hGetContents
    mapM_ putStrLn $ filter (/="") $ map (unpack . toLower) $ extract $ parseTags txt
