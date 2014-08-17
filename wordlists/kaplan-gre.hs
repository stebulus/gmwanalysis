import Data.Text (Text, toLower, unpack)
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO
import Text.HTML.TagSoup

extract :: [Tag Text] -> [Text]
extract [] = []
extract tags =
    let (spanElem, closeTagEtc) =
            break (~== "</span>")
            $ dropWhile (~/= "<span class='qWord lang-en'>") tags
    in innerText spanElem : extract (drop 1 closeTagEtc)

main = do
    filename:[] <- getArgs
    txt <- withFile filename ReadMode TIO.hGetContents
    mapM_ putStrLn $ filter (/="") $ map (unpack . toLower) $ extract $ parseTags txt
