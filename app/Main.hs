module Main where
import qualified BrainFuck as P
import Data.List
import System.Environment
import Control.Exception
exec (x:xs) = P.readAndExecBf x

data Args = Args {lang::String, path::String} deriving (Show)

parseArgs :: [String] -> (Args, [String])
parseArgs arg = ((Args "" ""), arg)

getLang :: [String] -> (Args, [String])
getLang arg = if "ook" `elem` arg
  then ((Args "ook" ""), (delete "ook" arg))
  else ((Args "bf" ""), (delete "bf" arg))

getPath :: (Args, [String]) -> Args
getPath ((Args lang _), arr) = if length arr /= 1
                                    then error "Problems with Argiments"
                                    else Args lang (arr !! 0)

execLang (Args lang path) = if lang == "bf"
  then P.readAndExecBf path
  else P.readAndExecOok path

main :: IO ()
main = do
          a <- getArgs
          let b = (getPath $ getLang a)
          execLang b
