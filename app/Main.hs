module Main where

-- import Lib
import System.IO
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Base16 as Base16
import Data.ByteString.Base64 as Base64
import Data.Digest.Pure.MD5
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "string: "
    str <- getLine
    putStr "service name: "
    name <- getLine
    putStr "e-mail: "
    email <- getLine
    putStr "key: "
    key <- getLine
    putStr "length(default: 14): "
    l <- getLine

    let s = BS.pack $ str ++ "\"" ++ name ++ "\"" ++ email
    -- let s = BS.pack str

    -- print s

    -- let hs = BS.fromStrict $ md5DigestBytes $ md5 s

    -- print hs
    let hs = nTimes (read key) (BS.fromStrict . Base16.encode . md5DigestBytes . md5) s

    putStr "hash: "
    -- BS.putStrLn $ BS.fromStrict $ Base16.encode $ BS.toStrict hs
    BS.putStrLn hs

    let bs64 = BS.fromStrict $ Base64.encode $ fst $ Base16.decode $ BS.toStrict hs
    putStr "base64: "
    BS.putStrLn bs64

    putStr "password: "
    BS.putStrLn $ BS.take (fromJust $ maybeRead l <|> Just 14) bs64



nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

maybeRead = fmap fst . listToMaybe . reads
