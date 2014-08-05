import qualified Data.HashSet as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Maybe
import System.Environment

type Page = [T.Text]

partition :: (a -> Bool) -> [a] -> [[a]]
partition _ [] = []
partition pred xs =
    let (chunk, rest) = break pred xs
    in chunk : partition pred (drop 1 rest)

breakPages :: [T.Text] -> [Page]
breakPages = partition (== T.empty)

withXmlKey :: String -> T.Text -> Maybe T.Text
withXmlKey k txt =
    let (key, eqVal) = T.break (== '=') txt
    in if key == T.pack k
         then Just $ T.drop 1 eqVal
         else Nothing
pageDatum :: Int -> String -> Page -> Maybe String
pageDatum n k page = fmap T.unpack $ withXmlKey k (page!!n)
title = pageDatum 0 "/title"
namespace = pageDatum 1 "/ns"
revisiontext :: Page -> T.Text
revisiontext page =
    T.intercalate (T.pack " ")
    $ map fromJust
    $ filter isJust
    $ map (withXmlKey "/revision/text")
    $ page

loadWords :: FilePath -> IO (S.HashSet String)
loadWords path = do
    str <- readFile path
    return $ S.fromList $ lines str

main = do
    args <- getArgs
    words <- case args of
        [wordFilePath] -> loadWords wordFilePath
        otherwise -> error "specify one file on command line"
    pages <- TIO.getContents
    mapM_ (\page -> do
            putStr $ fromJust $ title page
            putStr " "
            putStrLn $ T.unpack $ revisiontext page)
        $ filter (\page ->
            namespace page == Just "0"
            && fmap (`S.member` words) (title page) == Just True)
        $ breakPages
        $ T.lines
        $ pages
