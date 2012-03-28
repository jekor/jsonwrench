import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Aeson (encode, json, Value(..), object)
import Data.Aeson.Parser (value)
import Data.Attoparsec.Combinator (many1)
import Data.Attoparsec.ByteString.Lazy (parse, Result(..))
import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Lazy (unions)
import Data.List (intercalate)
import Data.Map (fromList)
import Data.Text (pack)
import Data.Text.Lazy (lines)
import Data.Text.Lazy.IO (getContents)
import qualified Data.Vector as V
import System.Environment (getArgs)

import Prelude hiding (getContents, putStrLn, lines)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- No arguments? Convert input into a JSON string.
    [] -> getContents >>= BS.putStrLn . encode
    ["lines"] -> getContents >>= BS.putStrLn . encode . lines
    ["ziplines"] -> do
       ls <- lines <$> getContents
       if even $ length ls
          then let (ks, vs) = splitAt ((length ls) `div` 2) ls in
               BS.putStrLn $ encode $ fromList $ zip ks vs
          else error "ziplines requires an even number of input lines"
    ["array"] -> do
       js' <- parse (many1 value) <$> BS.getContents
       case js' of
         Done _ js -> BS.putStrLn $ encode js
         Fail _ _ err -> error err
    ["zip"] -> do
       js' <- parse (many1 json) <$> BS.getContents
       case js' of
         Done _ [Array ks', Array vs] -> do
           let ks = map (\k -> case k of
                                 String str -> str
                                 _          -> error "zip requires the first JSON array to contain only strings") $ V.toList ks'
           BS.putStrLn $ encode $ object $ zip ks $ V.toList vs
         Done _ _ -> error "zip requires 2 JSON arrays"
         Fail _ _ err -> error err
    ["concat"] -> do
       js' <- parse (many1 json) <$> BS.getContents
       case js' of
         Done _ js -> do
           let as = map (\a -> case a of
                                 Array arr -> arr
                                 _          -> error "merge requires all arguments to be JSON arrays") js
           BS.putStrLn $ encode $ V.concat as
         Fail _ _ err -> error err
    ["merge"] -> do
       js' <- parse (many1 json) <$> BS.getContents
       case js' of
         Done _ js -> do
           let os = map (\o -> case o of
                                 Object obj -> obj
                                 _          -> error "merge requires all arguments to be JSON objects") js
           BS.putStrLn $ encode $ unions $ reverse os
         Fail _ _ err -> error err
    ("name":ss) -> do
       when (ss == []) $ error "jw name <key name>"
       j' <- parse value <$> BS.getContents
       case j' of
         Done _ j -> let name = pack $ intercalate " " ss in
                     BS.putStrLn $ encode $ object [(name, j)]
         Fail _ _ err -> error err
    _  -> error "jw [command]"