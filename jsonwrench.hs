import Control.Monad (when)
import Data.Aeson (encode, json, Value(..), object)
import Data.Aeson.Parser (value)
import Data.Attoparsec.Char8 (skipSpace)
import Data.Attoparsec.Combinator (many1, manyTill)
import Data.Attoparsec.ByteString.Lazy (parse, Result(..), endOfInput)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (lines)
import qualified Data.HashMap.Lazy as HM
import Data.Map (fromList)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Vector as V
import System.Environment (getArgs)

import Prelude hiding (getContents, putStrLn, lines)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["string"] -> BL.getContents >>= BL.putStrLn . encode . decodeUtf8

    ["unstring"] -> do
       j' <- parse (skipSpace *> value <* skipSpace <* endOfInput) <$> BL.getContents
       case j' of
         Done _ (String s) -> BS.putStr (encodeUtf8 s)
         Done _ _ -> error "unstring requires a JSON string"
         Fail _ _ err -> error err

    ["lines"] -> BL.getContents >>= BL.putStrLn . encode . map decodeUtf8 . lines

    ["ziplines"] -> do
       ls <- map decodeUtf8 . lines <$> BL.getContents
       if even $ length ls
          then let (ks, vs) = splitAt ((length ls) `div` 2) ls in
               BL.putStrLn $ encode $ fromList $ zip ks vs
          else error "ziplines requires an even number of input lines"

    ["unlines"] -> do
       j' <- parse json <$> BL.getContents
       case j' of
         Done _ (Array a) -> do
           let ss = V.map (\s -> case s of
                                   String str -> str
                                   _          -> error "unlines requires the JSON array to contain only strings") a
           V.mapM_ (BS.putStrLn . encodeUtf8) ss
         Done _ _ -> error "unarray requires a JSON array"
         Fail _ _ err -> error err

    ["array"] -> do
       js' <- parse (skipSpace *> manyTill (value <* skipSpace) endOfInput) <$> BL.getContents
       case js' of
         Done _ js -> BL.putStrLn $ encode js
         Fail _ _ err -> error err

    ["unarray"] -> do
       j' <- parse json <$> BL.getContents
       case j' of
         Done _ (Array a) -> V.mapM_ (BL.putStrLn . encode) a
         Done _ _ -> error "unarray requires a JSON array"
         Fail _ _ err -> error err

    ["zip"] -> do
       js' <- parse (many1 json) <$> BL.getContents
       case js' of
         Done _ [Array ks', Array vs] -> do
           let ks = V.map (\k -> case k of
                                   String str -> str
                                   _          -> error "zip requires the first JSON array to contain only strings") ks'
           BL.putStrLn $ encode $ object $ V.toList $ V.zip ks vs
         Done _ _ -> error "zip requires 2 JSON arrays"
         Fail _ _ err -> error err

    ["concat"] -> do
       js' <- parse (many1 json) <$> BL.getContents
       case js' of
         Done _ js -> do
           let as = map (\a -> case a of
                                 Array arr -> arr
                                 _          -> error "merge requires all arguments to be JSON arrays") js
           BL.putStrLn $ encode $ V.concat as
         Fail _ _ err -> error err

    ["merge"] -> do
       js' <- parse (many1 json) <$> BL.getContents
       case js' of
         Done _ js -> do
           let os = map (\o -> case o of
                                 Object obj -> obj
                                 _          -> error "merge requires all arguments to be JSON objects") js
           BL.putStrLn $ encode $ HM.unions $ reverse os
         Fail _ _ err -> error err

    ["normalize"] -> do
       j' <- parse value <$> BL.getContents
       case j' of
         Done _ j -> BL.putStrLn $ encode j
         Fail _ _ err -> error err

    ["name",name] -> do
       j' <- parse value <$> BL.getContents
       case j' of
         Done _ j -> BL.putStrLn $ encode $ object [(pack name, j)]
         Fail _ _ err -> error err
    ("name":_) -> error "jw name <key name>"

    ["lookup",name] -> do
       j' <- parse value <$> BL.getContents
       case j' of
         Done _ (Object o) -> BL.putStrLn $ encode $ HM.lookupDefault Null (pack name) o
         Done _ _ -> error "lookup requires a JSON object"
         Fail _ _ err -> error err
    ("lookup":_) -> error "jw lookup <key name>"

    ["insert",name] -> do
       j1 <- parse (skipSpace *> value) <$> BL.getContents
       case j1 of
         Done s (Object o) ->
           case parse (skipSpace *> value <* skipSpace <* endOfInput) s of
             Done _ v -> BL.putStrLn $ encode $ HM.insert (pack name) v o
             Fail _ _ err -> error err
         Done _ _ -> error "insert requires a JSON object first"
         Fail _ _ err -> error err

    ("drop":keys) -> do
       when (keys == []) $ error "jw drop <key name(s)>"
       j' <- parse (skipSpace *> value <* skipSpace <* endOfInput) <$> BL.getContents
       case j' of
         Done _ (Object o) -> BL.putStrLn $ encode $ foldr HM.delete o $ map pack keys
         Done _ _ -> error "drop requires a JSON object"
         Fail _ _ err -> error err

    ("take":keys) -> do
       when (keys == []) $ error "jw take <key name(s)>"
       j' <- parse (skipSpace *> value <* skipSpace <* endOfInput) <$> BL.getContents
       case j' of
         Done _ (Object o) -> BL.putStrLn $ encode $ HM.intersection o $ foldr (\k o' -> HM.insert k "" o') HM.empty $ map pack keys
         Done _ _ -> error "take requires a JSON object"
         Fail _ _ err -> error err

    _  -> error "jw [command]"
